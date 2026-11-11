# Common functions across multiple scripts

### LF file helper -------------------------------------------------------------

# Read in a LANDFIRE raster
# Taking advantage of standard Landfire folder and naming schemes
get_LF_raster <- function(lf_var, folder_lfdata_base, version_target) {
  # fbfm40 : handle common short/variant names
  fm_vars <- c("fbfm40", "FBFM40", "fm40", "FM40", "fbfm", "FBFM")
  lf_var <- if_else(lf_var %in% fm_vars, "FBFM40", lf_var)
  # handle both lower or upper case input
  lf_var_upper <- toupper(lf_var)

  # BPS 2020 has a different naming scheme than other LF layers
  #
  if (lf_var_upper == "BPS") {
    folder_bps <- list.files(
      path = folder_lfdata_base,
      #version_bps
      pattern = paste0("^LF", version_target, "_BPS_", "[0-9]*", "_CONUS$"),
      include.dirs = TRUE,
      full.names = TRUE
    )
    file_bps <- list.files(
      path = file.path(folder_bps, "Tif"),
      pattern = paste0("\\.tif$"),
      full.names = TRUE
    )
    # Note: we are using the Raster values (in rules too)
    # R will default and perhaps show BPS_MODEL instead but math is on raster value
    this_rast <- terra::rast(file_bps)
  } else {
    # match file name
    this_file <- list.files(
      path = folder_lfdata_base,
      pattern = paste0(
        "^LF",
        version_target,
        "_",
        lf_var_upper,
        "_CONUS\\.tif$"
      ),
      recursive = TRUE,
      full.names = TRUE
    )
    this_rast <- terra::rast(this_file)
  }
  this_rast
}

### Spatial --------------------------------------------------------------------

clip_by_aoi <- function(my_rast, aoi) {
  # expecting my_rast as a spatRaster, my_aoi as a spatVector
  ## first check if AOI is covered by raster
  ##  if not, extend() first (so has NAs rather than failing)
  # switch to sf to use st_covered_by()
  aoi_ext_sf <- terra::ext(aoi) %>% terra::vect() %>% sf::st_as_sf()
  rast_ext_sf <- terra::ext(my_rast) %>% terra::vect() %>% sf::st_as_sf()
  # covered_by returns sparse geometry binary predicate, extracting TRUE/FALSE
  if (as.matrix(sf::st_covered_by(aoi_ext_sf, rast_ext_sf))[1, 1]) {
    # if true, then aoi in within/boundary of raster, and do not need to extend
    rast_to_clip <- my_rast
  } else {
    # if false, then extend the raster to aoi before clipping
    # this adds NAs rather than failing with an error
    rast_to_clip <- terra::extend(my_rast, aoi)
    # Return warning message
    # TODO Consider also passing in some kind of name to get more descriptive
    warning("The AOI had larger extents than the raster.")
  }
  # clip fuels to EXTENT of AOI (mask = FALSE)
  rast_clip <- terra::crop(rast_to_clip, aoi, mask = FALSE)
}

### Preprocessing --------------------------------------------------------------

get_fv_xwalk <- function(
  fv_var_name = c("fvc", "fvh", "fvt"),
  folder_xwalk,
  version_target
) {
  # need lower case here
  fv_var_name <- tolower(fv_var_name)
  # get fv xwalk crosswalk file, programmatically
  # created in 1_ruleset 0_encoding_preprocess.R
  file_fv_xwalk <- list.files(
    path = folder_xwalk,
    pattern = paste0("^LF", version_target, "_", fv_var_name, "_xwalk\\.csv$"),
    full.names = TRUE
  )
  # read in xwalk and create rcl matrix
  field_name <- paste0(fv_var_name, "_ufid")
  this_rcl <- readr::read_csv(file_fv_xwalk, show_col_types = FALSE) %>%
    # fun with variable variable name. will be "fv{x}_ufid"
    # selecting and renaming in one step
    dplyr::select(c(IS = VALUE, BECOMES = tidyselect::all_of(field_name))) %>%
    as.matrix()
}

process_fv <- function(
  fv_var = c("FVC", "FVH", "FVT"),
  folder_xwalk,
  folder_lfdata_base,
  folder_out,
  version_target,
  aoi
) {
  # accept both upper and lower versions fv_var
  fv_var_lower <- tolower(fv_var)
  fv_var_upper <- toupper(fv_var)
  ## Set up crosswalk and raster
  # get xwalk
  this_rcl <- get_fv_xwalk(
    fv_var_name = fv_var_lower,
    folder_xwalk,
    version_target
  )
  # get raster
  this_orig <- get_LF_raster(fv_var, folder_lfdata_base, version_target)
  # clip raster to AOI
  this_clip <- clip_by_aoi(this_orig, aoi)
  ## Reclass / FV* encoding (first of the double encoding)
  # others = -1 to see if any did not reclass correctly
  this_fv <- terra::classify(this_clip, rcl = this_rcl, others = -1)
  # test to see if -1 has shown up, stop if so
  this_min <- terra::global(this_fv, min, na.rm = TRUE)
  # time notes: global() 2 min; min() 7 min; # app() almost 10 min
  if (this_min == -1) {
    stop(paste0(fv_var_upper, " classification failed some values."))
  }
  # rename
  name_new <- paste0("LF", version_target, "_", fv_var_lower, "_encoded")
  varnames(this_fv) <- name_new
  names(this_fv) <- name_new
  ## Save raster
  terra::writeRaster(
    this_fv,
    file.path(
      folder_out,
      paste0("LF", version_target, "_", fv_var_lower, "_coded.tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

# Process and prep original Landfire fuel rasters
# Clips to AOI
# Replaces -1111 and -9999 (no data values) as NA
# Saves out result under per project data folder
process_fuel <- function(
  fuel_var = c("CC", "CH", "CBD", "CBH", "FBFM40"),
  folder_xwalk,
  folder_lfdata_base,
  folder_out,
  version_target,
  aoi
) {
  ## Handle fuel variables names
  # fbfm40 : handle common short/variant names
  fm_vars <- c("fbfm40", "FBFM40", "fm40", "FM40", "fbfm", "FBFM")
  fuel_var <- if_else(
    fuel_var %in% fm_vars,
    "FBFM40",
    fuel_var
  )
  # accept both upper and lower versions fv_var
  fuel_var_lower <- tolower(fuel_var)
  fuel_var_upper <- toupper(fuel_var)
  ## Raster and clip
  this_orig <- get_LF_raster(fuel_var, folder_lfdata_base, version_target)
  # clip raster to AOI
  this_clip <- clip_by_aoi(this_orig, aoi)
  ## Reclassify some variables
  # Make -1111 and -9999 into NAs
  this_clip <- terra::subst(
    this_clip,
    from = c(-9999, -1111),
    to = c(NA, NA),
    others = NULL #keep other values as they are
  )
  ## Save out
  terra::writeRaster(
    this_clip,
    file.path(
      folder_out,
      paste0("LF", version_target, "_", fuel_var_lower, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

# Similar to fuel processing, but different enough for its own function
process_bps <- function(
  folder_xwalk,
  folder_lfdata_base,
  folder_out,
  version_bps,
  aoi
) {
  ## Raster and clip
  bps <- get_LF_raster(
    lf_var = "BPS",
    folder_lfdata_base = folder_lfdata_base,
    # important to use bps version which is likely different from fuel version
    version_bps = version_bps
  )
  ## Note: BPS extents changed between 2020 and 2024
  # clip_by_aoi should negate the need to do extending here
  #bps_ext <- terra::extend(bps, fvh)
  bps_clip <- clip_by_aoi(bps, aoi)
  # IS ALLOWED TO HAVE -1111, there are rules for it,
  #   but need to recode for encoding criteria (new value 8888)
  # remove -9999 (to NA)
  bps_clip <- terra::subst(
    bps_clip,
    from = c(-9999, -1111),
    to = c(NA, 8888),
    others = NULL #keep other values as they are
  )
  # save out
  terra::writeRaster(
    bps_clip,
    file.path(folder_out, paste0("LF", version_bps, "_bps_coded.tif")),
    gdal = c("COMPRESS=DEFLATE")
  )
}


### Regression-related ---------------------------------------------------------

#Helper function for regression reclassification matrix
make_reg_rcl <- function(reg_tbl, col_name) {
  reg_tbl %>%
    #using all_of() because of external vector in selection
    #  (col_name is a variable, with the value of the column name)
    dplyr::select(from_code, tidyselect::all_of(col_name)) %>%
    #very important to reduce rcl length for reclassifying time
    dplyr::distinct() %>%
    as.matrix()
}

#CBH Regression function
# Calculates the regression update values
#  to be adjusted later, per variable
# TODO Dev - references tables read in the 4_CC_CH.R & 5_CBD_CBH.R scripts
#      Dev - calc rasters include
#            All: r_intercept, r_height_scale, r_cover_scale
#                 (calc'd from enc_rast_dist and tables above)
#            CC/CH: height_mid, cover_mid
#            CBH: new CH, new CC
calc_reg_raw <- function(var_layer = c("cc", "ch", "cbh")) {
  if (var_layer == "cc") {
    this_tbl <- cover_reg_tbl
  } else if (var_layer == "ch") {
    this_tbl <- height_reg_tbl
  } else if (var_layer == "cbh") {
    this_tbl <- cbh_reg_tbl
  }

  #set up encoding guide
  this_tbl <- this_tbl %>%
    dplyr::mutate(from_code = HDist * 1e4 + EVT_Fill * 1e0)

  #set up reclassification matrices
  rcl_intercept <- make_reg_rcl(reg_tbl = this_tbl, col_name = "intercept")
  rcl_height <- make_reg_rcl(reg_tbl = this_tbl, col_name = "HT_coef")
  rcl_cover <- make_reg_rcl(reg_tbl = this_tbl, col_name = "CC_coef")

  #get rasters of the coefficients for regression
  r_intercept <- terra::classify(
    enc_rast_dist,
    rcl = rcl_intercept,
    others = NA
  )
  r_height_scale <- terra::classify(
    enc_rast_dist,
    rcl = rcl_height,
    others = NA
  )
  r_cover_scale <- terra::classify(enc_rast_dist, rcl = rcl_cover, others = NA)

  # NOTE: Regression piece rasters can have fewer pixels than disturbed
  #        (i.e. be a subset), as some DIST-EVT pairs are not in reg tables

  # Regress stack and calc
  if (var_layer %in% c("cc", "ch")) {
    # formula: intercept + height_scale * fvh midpoint + cover_scale * fvc midpoint
    reg_stack <- c(
      r_intercept,
      r_height_scale * height_mid,
      r_cover_scale * cover_mid
    )
    reg_rast <- terra::app(reg_stack, fun = "sum")
  } else if (var_layer == "cbh") {
    # formula: intercept + height_scale * new ch + cover_scale * new cc
    (start_time_rr <- Sys.time())
    reg_stack <- c(
      r_intercept,
      r_height_scale * ch_mid_new,
      r_cover_scale * cc_new
    )
    reg_rast <- terra::app(reg_stack, fun = "sum")
    (end_time_rr <- Sys.time())
    (end_time_rr - start_time_rr)
  }

  return(reg_rast)
}
