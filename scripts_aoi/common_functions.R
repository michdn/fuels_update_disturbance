# Common functions across multiple scripts

### File helpers ---------------------------------------------------------------

# Read in a LANDFIRE raster
# Taking advantage of standard Landfire folder and naming schemes
get_LF_raster <- function(lf_var, folder_lfdata_base, version_target) {
  # fbfm40 : handle common short/variant names
  fm_vars <- c("fbfm40", "FBFM40", "fm40", "FM40", "fbfm", "FBFM")
  lf_var <- dplyr::if_else(lf_var %in% fm_vars, "FBFM40", lf_var)
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

# Function to get preprocessed raster in project folder. like get_LF_raster() but
#  for preprocessed data. More variety of vars: coded and midpoint rasters
get_preproc_raster <- function(
  lf_proc_var = c(
    "bps_coded",
    "cbd",
    "cbh",
    "cc",
    "ch",
    "fbfm40",
    "fvc_coded",
    "fvc_midpoints",
    "fvh_coded",
    "fvh_midpoints",
    "fvt_coded"
  ),
  folder_lfproc_aoi,
  version_target
) {
  # fbfm40 : handle common short/variant names
  fm_vars <- c("fbfm40", "FBFM40", "fm40", "FM40", "fbfm", "FBFM")
  lf_proc_var <- dplyr::if_else(lf_proc_var %in% fm_vars, "fbfm40", lf_proc_var)
  # handle both lower or upper case input
  lf_proc_var_lower <- tolower(lf_proc_var)

  # Find target in pre-processed LANDFIRE data at project-level
  this_file <- list.files(
    path = folder_lfproc_aoi,
    pattern = paste0("^LF", version_target, "_", lf_proc_var_lower, "\\.tif$"),
    full.names = TRUE
  )
  this_rast <- terra::rast(this_file)
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
