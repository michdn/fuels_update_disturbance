# Set up DIST & criteria encoded rasters
#  Rule-encoded raster: used in both fbfm and canopy guide scripts
#  Regression-encoded raster: used in canopy-layer regression scripts

# ~3 hours preprocessing step

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

source(file.path("scripts", "common_vars_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

### Settings -------------------------------------------------------------------

folder_out <- file.path(folder_out_base, version_proj, "processing")
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

#preventing scientific notation
options(scipen = 999)

#To avoid issues with large numbers, tell terra to use
#  64 bit numbers, otherwise will see weird rounding on the
#  very large encoded numbers.
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terra::terraOptions(datatype = "FLT8S") #FLT8S

### Data in --------------------------------------------------------------------

# Disturbance
# Set in the project parameters
dist <- terra::rast(dist_file)

# Find target based on version_target
#  in pre-processed LANDFIRE data
file_bps <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_bps, "_bps_coded\\.tif$"),
  full.names = TRUE
)
bps <- terra::rast(file_bps)

# Veg
# per GEE code:
# "the actual values being used in the FM40 crosswalk are the FVH, FVC, FVT
# however, the tables have EVH, EVC, EVT...
# so variables are named as in the tables but note they are actually the F* layers"

file_evc <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvc_coded\\.tif$"),
  full.names = TRUE
)
file_evh <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvh_coded\\.tif$"),
  full.names = TRUE
)
file_evt <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvt_coded\\.tif$"),
  full.names = TRUE
)
evc <- terra::rast(file_evc)
evh <- terra::rast(file_evh)
evt <- terra::rast(file_evt)

# UNcoded EVT for regression rast
# EVT (FVT)
file_fvt_lf <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVT_CONUS\\.tif$"),
  recursive = TRUE,
  full.names = TRUE
)
evt_lf <- terra::rast(file_fvt_lf)
# Handle -9999 in original LF raster
evt_lf <- terra::mask(evt_lf, evt_lf, maskvalues = -9999)

### LF Rule Encoding -----------------------------------------------------------

#42-70 min for stack & sum (depending on what else is running, chrome/RAM, etc)
# (faster than all in one calc by over 20 min)
# with DOUBLE encoding in these EV* variables
# fmt:skip
r_stack <- c(
  dist * 1e11, 
  bps  *  1e7, 
  evh  *  1e5, 
  evc  *  1e3, 
  evt  *  1e0)
r_coded <- terra::app(r_stack, fun = "sum")

## Preprocess Encoded where Disturbance

# Rather than masking each zone inside the loop.
#where no disturbance, NA, otherwise encoded value
#mask (17 min) is much faster than ifel (50 min)
coded_dist <- terra::mask(r_coded, dist, maskvalues = 0, inverse = FALSE)

# Save out
terra::writeRaster(
  coded_dist,
  file.path(folder_out, "encoded_rule_criteria_distonly.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  #set for largest possible values given size of encoded values
  datatype = "FLT8S"
)

### Regression DIST encoding ---------------------------------------------------

# ~1.5 hours

#calc encoded raster
# Uses original/un-encoded EVT
# This will match against the regression tables for calculating
#  updated canopy fuel layers
#fmt:skip
r_stack_reg <- c(
  dist   * 1e4, 
  evt_lf * 1e0
)
enc_reg_rast <- terra::app(r_stack_reg, fun = "sum")

#encoded rast only in disturbed areas
enc_reg_rast_dist <- terra::ifel(dist > 0, enc_reg_rast, NA)

# Save out
terra::writeRaster(
  enc_reg_rast_dist,
  file.path(folder_out, "encoded_regression_distonly.tif"),
  gdal = c("COMPRESS=DEFLATE")
)
