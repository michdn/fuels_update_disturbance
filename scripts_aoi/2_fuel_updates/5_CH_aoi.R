# Updates CC in AOI

# Only updates disturbed areas
# Uses canopy guide, FVC & FVH midpoints,
#      EVT (FVT), DIST, and
#      Pre-generated disturbance regression tables

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
source(file.path("scripts_aoi", "fuelupdate_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts_aoi", "0_parameters", "dev_trpa_aoi.R"))

#save a copy of only-updated disturbed pixels (does not affect final output)
save_updatedonly <- TRUE

### Data in --------------------------------------------------------------------

# from 1_dist_criteria_encoding_aoi.R
enc_rast_dist <- terra::rast(file.path(
  folder_out_base,
  version_proj,
  "processing",
  "encoded_regression_distonly.tif"
))

## Raster data for calculations
# Canopy guide
file_cg <- file.path(
  folder_out_base,
  version_proj,
  "processing",
  paste0("canopy_guide_", version_proj, ".tif")
)
cg <- terra::rast(file_cg)

# Disturbance
# Set in the project parameters
dist_orig <- terra::rast(dist_file)
dist <- clip_by_aoi(dist_orig, aoi)
#binary version for masking needs
dist_bin <- terra::ifel(dist > 0, 1, NA)

# preprocessed project folder (created in 0_preprocess_aoi.R)
folder_lfproc_aoi <- file.path(
  folder_out_base,
  version_proj,
  "lf_processed_aoi"
)

# CH uses both
cc_orig <- get_preproc_raster(
  lf_proc_var = "cc",
  folder_lfproc_aoi,
  version_target
)
ch_orig <- get_preproc_raster(
  lf_proc_var = "ch",
  folder_lfproc_aoi,
  version_target
)

### CH regress & update --------------------------------------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image.

ch_reg_base <- calc_reg_base(
  var_layer = "ch",
  enc_rast_dist = enc_rast_dist,
  folder_lfproc_aoi = folder_lfproc_aoi,
  version_target = version_target
)

# Min 0 clamp. MUST happen first before binning
ch_min0 <- terra::clamp(ch_reg_base, lower = 0, upper = Inf, values = TRUE)

# Bin it to LandFire CH bin values
# Multiply by 10 (as LF CH is meters * 10)
rcl_ch_bins <- tibble::tribble(
  ~ch_from , ~ch_to , ~ch_bin ,
   0       ,  0.18  ,       0 ,
   0.18    ,  5     ,      30 ,
   5       ,  9     ,      70 ,
   9       , 13     ,     110 ,
  13       , 17     ,     150 ,
  17       , 21     ,     190 ,
  21       , 25     ,     230 ,
  25       , 29     ,     270 ,
  29       , 33     ,     310 ,
  33       , 37     ,     350 ,
  37       , 41     ,     390 ,
  41       , 45     ,     430 ,
  45       , 50     ,     470 ,
  50       , Inf    ,     510
) %>%
  as.matrix()
ch_bin <- terra::classify(
  ch_min0,
  rcl = rcl_ch_bins,
  include.lowest = TRUE,
  right = FALSE
)

# Zero out where canopy guide is 0
ch_cg0 <- terra::mask(ch_bin, cg, maskvalues = 0, updatevalue = 0)

# Zero out where existing CC (yes, cover, as in GEE code) is 0
ch_cc0 <- terra::mask(ch_cg0, cc_orig, maskvalues = 0, updatevalue = 0)

# Limit to ONLY disturbed pixels
ch_updtdist <- terra::mask(ch_cc0, dist_bin, maskvalues = 1, inverse = TRUE)

# Rename
names(ch_updtdist) <- "ch"
varnames(ch_updtdist) <- "ch"

if (save_updatedonly) {
  folder_ud <- file.path(folder_out_base, version_proj, "distonly_update")
  dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    ch_updtdist,
    file.path(
      folder_ud,
      paste0("ch_updtonly_", version_proj, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

## Update existing - CH

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not disturbed & regression calc'd)
ch_updt <- terra::cover(ch_updtdist, ch_orig, values = NA)

#Save
terra::writeRaster(
  ch_updt,
  file.path(
    folder_out_base,
    version_proj,
    paste0("ch_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
