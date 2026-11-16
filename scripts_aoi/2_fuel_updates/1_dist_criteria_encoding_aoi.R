# Set up DIST & criteria encoded rasters
#  Rule-encoded raster: used in both fbfm and canopy guide scripts
#  Regression-encoded raster: used in canopy-layer regression scripts

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

source(file.path("scripts_aoi", "common_variables.R"))
source(file.path("scripts_aoi", "common_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts_aoi", "0_parameters", "dev_trpa_aoi.R"))

### Settings -------------------------------------------------------------------

folder_out <- file.path(folder_out_base, version_proj, "processing")
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

# preprocessed project folder (created in 0_preprocess_aoi.R)
folder_lfproc_aoi <- file.path(
  folder_out_base,
  version_proj,
  "lf_processed_aoi"
)

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
dist_orig <- terra::rast(dist_file)
dist <- clip_by_aoi(dist_orig, aoi)

bps <- get_preproc_raster(
  lf_proc_var = "bps_coded",
  folder_lfproc_aoi = folder_lfproc_aoi,
  # BPS Version
  version_target = version_bps
)

# Veg
# per GEE code:
# "the actual values being used in the FM40 crosswalk are the FVH, FVC, FVT
# however, the tables have EVH, EVC, EVT...
# so variables are named as in the tables but note they are actually the F* layers"
evc <- get_preproc_raster(
  lf_proc_var = "fvc_coded",
  folder_lfproc_aoi = folder_lfproc_aoi,
  version_target = version_target
)
evh <- get_preproc_raster(
  lf_proc_var = "fvh_coded",
  folder_lfproc_aoi = folder_lfproc_aoi,
  version_target = version_target
)
evt <- get_preproc_raster(
  lf_proc_var = "fvt_coded",
  folder_lfproc_aoi = folder_lfproc_aoi,
  version_target = version_target
)

# UNcoded EVT for regression rast
# EVT (FVT)
evt_lf_orig <- get_LF_raster(
  lf_var = "FVT",
  folder_lfdata_base = folder_lfdata_base,
  version_target = version_target
)
# clip raster to AOI
evt_lf_aoi <- clip_by_aoi(evt_lf_orig, aoi)
# Handle -9999 in original LF raster
evt_lf <- terra::mask(evt_lf_aoi, evt_lf_aoi, maskvalues = -9999)

### LF Rule Encoding -----------------------------------------------------------

# stack & sum is faster than all in one calc
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
#  make NA where no disturbance, otherwise encoded value
# GEE output will have background value of 0,
#  others (like created through the LF annual dist scripts here) may just have
#  NA where no disturbance
coded_dist <- terra::ifel(dist > 0, r_coded, NA)

names(coded_dist) <- "criteria"
varnames(coded_dist) <- "criteria"

# Save out
terra::writeRaster(
  coded_dist,
  file.path(folder_out, "encoded_rule_criteria_distonly.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  #set for largest possible values given size of encoded values
  datatype = "FLT8S"
)

### Regression DIST encoding ---------------------------------------------------

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

names(enc_reg_rast_dist) <- "reg_enc"
varnames(enc_reg_rast_dist) <- "reg_enc"

# Save out
terra::writeRaster(
  enc_reg_rast_dist,
  file.path(folder_out, "encoded_regression_distonly.tif"),
  gdal = c("COMPRESS=DEFLATE")
)
