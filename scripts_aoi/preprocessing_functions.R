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
  this_orig <- get_LF_raster(fv_var_upper, folder_lfdata_base, version_target)
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
    version_target = version_bps
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

calc_midpoints <- function(
  fv_var = c("FVC", "FVH"),
  folder_lfdata_base,
  version_target,
  folder_lut,
  folder_out
) {
  # accept both upper and lower versions fv_var
  fv_var_lower <- tolower(fv_var)
  fv_var_upper <- toupper(fv_var)
  # get raster
  this_fv_r <- get_LF_raster(fv_var_upper, folder_lfdata_base, version_target)
  # get look up table
  if (fv_var_upper == "FVC") {
    this_lut <- readr::read_csv(
      file.path(
        folder_lut,
        "LF2020_LUT_Cover.csv"
      ),
      show_col_types = FALSE
    )
    # Create reclassification guide
    this_rcl <- this_lut %>%
      dplyr::select(EVC, MidPoint) %>%
      as.matrix()
  } else if (fv_var_upper == "FVH") {
    this_lut <- readr::read_csv(
      file.path(
        folder_lut,
        "LF2020_LUT_Height.csv"
      ),
      show_col_types = FALSE
    )
    this_rcl <- this_lut %>%
      dplyr::select(EVH, MidPoint) %>%
      as.matrix()
  } else {
    stop("Unknown midpoint FV* variable name")
  }
  # AOI
  this_fv_clip <- clip_by_aoi(this_fv_r, aoi)
  # Reclassify
  # -9999 is not explicitly handled, but becomes NA in classifying
  this_mid <- terra::classify(this_fv_clip, rcl = this_rcl, others = NA)
  names(this_mid) <- paste0(fv_var_upper, "_MIDPOINT")
  varnames(this_mid) <- paste0(fv_var_upper, "_MIDPOINT")
  #Save out
  terra::writeRaster(
    this_mid,
    file.path(
      folder_out,
      paste0("LF", version_target, "_", fv_var_lower, "_midpoints.tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}
