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

cc_orig <- get_preproc_raster(
  lf_proc_var = "cc",
  folder_lfproc_aoi,
  version_target
)

### CC regress & update --------------------------------------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image.
cc_reg_base <- calc_reg_base(
  var_layer = "cc",
  enc_rast_dist = enc_rast_dist,
  folder_lfproc_aoi = folder_lfproc_aoi,
  version_target = version_target
)

# Clamp value between 0 and 100.
# Values = TRUE transform values outside bounds to the limit values
# This was done in GEE python.
# A LF2024 test run had values -19 to 96 before clamping.
cc_reg <- terra::clamp(cc_reg_base, lower = 0, upper = 100, values = TRUE)

# Bin it like LF CC
rcl_cc_bins <- tibble::tibble(
  cc_from = seq(0, 90, by = 10),
  cc_to = seq(10, 100, by = 10),
  cc_bin = c(0, seq(15, 95, by = 10))
) %>%
  as.matrix()
cc_reg_bin <- terra::classify(
  cc_reg,
  rcl = rcl_cc_bins,
  include.lowest = TRUE,
  right = FALSE
)

# Zero out where canopy guide is 0 (will add extra 0s outside of disturbed-regressed)
cc_reg_cg0 <- terra::mask(cc_reg_bin, cg, maskvalues = 0, updatevalue = 0)

# Zero out where existing CC is 0 (will add extra 0s outside of disturbed-regressed)
cc_reg_cc0 <- terra::mask(cc_reg_cg0, cc_orig, maskvalues = 0, updatevalue = 0)
# #DEV checks - may have increasing number of pixels with value 0 (def w/ cg).
# DEV Note: The dist masks ones will still have different total number of pixels!
#   Not all disturbed pixels will be able to have a regression-value calculated
#   Canopy guide is not able to be calculated everywhere that CC exists.

# So now limit to ONLY disturbed pixels
cc_updtdist <- terra::mask(cc_reg_cc0, dist_bin, maskvalues = 1, inverse = TRUE)

# Rename
names(cc_updtdist) <- "cc"
varnames(cc_updtdist) <- "cc"

if (save_updatedonly) {
  folder_ud <- file.path(folder_out_base, version_proj, "distonly_update")
  dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    cc_updtdist,
    file.path(
      folder_ud,
      paste0("cc_updtonly_", version_proj, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

## Update existing - CC

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not-disturbed-&-regression-calc'd)
cc_updt <- terra::cover(cc_updtdist, cc_orig, values = NA)

#Save
terra::writeRaster(
  cc_updt,
  file.path(
    folder_out_base,
    version_proj,
    paste0("cc_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
