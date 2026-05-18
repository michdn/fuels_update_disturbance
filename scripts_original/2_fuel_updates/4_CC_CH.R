# Updates CC & CH
# (Note: Runs CONUS-wide, not per zone)

# Only updates disturbed areas
# Uses canopy guide, FVC & FVH midpoints,
#      EVT (FVT), DIST, and
#      Pre-generated disturbance regression tables

# TODO
# DEV NOTE
# Python GEE split up CC & CH, and CBH & CBD as separate scripts
#  so that pattern was followed, though we may want to adjust
#  As the same enc_rast_dist raster is used, and it's created twice atm.
# The same calc_reg_raw() function can be used for cc, ch, and cbh
#   (with the if-elses in the local function).

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

#save a copy of only-updated disturbed pixels (does not affect final output)
save_updatedonly <- TRUE

### Data in --------------------------------------------------------------------

# from 1_dist_criteria_encoding.R
enc_rast_dist <- terra::rast(file.path(
  folder_out_base,
  version_proj,
  "processing",
  "encoded_regression_distonly.tif"
))

## Regression tables
# (From cloud storage. Created for v1, Carrie/Kyle/Kel?)
cover_reg_tbl <- readr::read_csv(file.path(
  folder_regtbl,
  "Cover_Disturbance_Tbl.csv"
))

height_reg_tbl <- readr::read_csv(file.path(
  folder_regtbl,
  "Height_Disturbance_Tbl.csv"
))

## Raster data for calculations

# Canopy guide
file_cg <- file.path(
  folder_out_base,
  version_proj,
  "processing",
  paste0("canopy_guide_conus_", version_proj, ".tif")
)
cg <- terra::rast(file_cg)

# Disturbance
# Set in the project parameters
dist <- terra::rast(dist_file)
#binary version for masking needs
dist_bin <- terra::ifel(dist > 0, 1, NA)

# FVC & FVH midpoints generated from 0b_canopy_midpoints.R
file_fvc_mid <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvc_midpoints\\.tif$"),
  full.names = TRUE
)
cover_mid <- terra::rast(file_fvc_mid)

file_fvh_mid <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvh_midpoints\\.tif$"),
  full.names = TRUE
)
height_mid <- terra::rast(file_fvh_mid)

# Existing CC and CH (processed)
file_cc <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cc\\.tif$"),
  full.names = TRUE
)
cc_orig <- terra::rast(file_cc)

file_ch <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_ch\\.tif$"),
  full.names = TRUE
)
ch_orig <- terra::rast(file_ch)

# # Zones to limit data (i.e. no LF buffer around CONUS)
# # appropriate zone raster, parameter set in parameter script file
# if (LF_buffer == 0) {
#   zones_r <- terra::rast(
#     file.path(folder_mapzones, "LF_zones_0km_rasterized.tif")
#   )
# } else if (LF_buffer == 90) {
#   zones_r <- terra::rast(
#     file.path(folder_mapzones, "LF_zones_90km_rasterized.tif")
#   )
# } else {
#   stop(paste0("Unmatched LF buffer selection: ", LF_buffer))
# }

### CC regress & update --------------------------------------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image.
# ~ 45m-1hr
cc_reg_raw <- calc_reg_raw("cc")

# Clamp value between 0 and 100.
# Values = TRUE transform values outside bounds to the limit values
# This was done in GEE python.
# A LF2024 test run has values -19 to 96.
cc_reg <- terra::clamp(cc_reg_raw, lower = 0, upper = 100, values = TRUE)

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

# #DEV test
# ccrb_freq <- freq(cc_reg_bin) %>% as_tibble() %>%
#   select(-layer) %>% mutate(vers="cc_reg_bin")

# Zero out where canopy guide is 0 (will add extra 0s outside of disturbed-regressed)
cc_reg_cg0 <- terra::mask(cc_reg_bin, cg, maskvalues = 0, updatevalue = 0)
# #DEV test
# cc_reg_cg0_d <- mask(cc_reg_cg0, dist_bin, maskvalues=1, inverse=TRUE)
# ccrg_freq <- freq(cc_reg_cg0_d) %>% as_tibble() %>%
#   select(-layer) %>% mutate(vers="cc_reg_cg0_d")

# Zero out where existing CC is 0 (will add extra 0s outside of disturbed-regressed)
cc_reg_cc0 <- terra::mask(cc_reg_cg0, cc_orig, maskvalues = 0, updatevalue = 0)
# #DEV test
# cc_reg_cc0_d <- mask(cc_reg_cc0, dist_bin, maskvalues=1, inverse=TRUE)
# ccrcc_freq <- freq(cc_reg_cc0_d) %>% as_tibble() %>%
#   select(-layer) %>% mutate(vers="cc_reg_cc0_d")

# #DEV checks - may have increasing number of pixels with value 0 (def w/ cg).
# (qa_freqs <- bind_rows(ccrb_freq, ccrg_freq, ccrcc_freq))
# write_csv(qa_freqs, file.path("data", "test_data", "qa_freqs.csv"))

# DEV Note: The dist masks ones will still have different total number of pixels!
#   Not all disturbed pixels will be able to have a regression-value calculated
#   Canopy guide is not able to be calculated everywhere that CC exists.
#

# Limit to ONLY disturbed pixels
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

#count pixels - same 9795530328
#global(cc_updt, "notNA")
#global(cc_orig, "notNA")

# Based on project parameters, potentially limit to zones_r, removing LF buffer
if (LF_buffer == 0) {
  cc_conus <- terra::ifel(!is.na(zones0km_r), cc_updt, NA)
} else {
  cc_conus <- cc_updt
}

#Save
terra::writeRaster(
  cc_conus,
  file.path(
    folder_out_base,
    version_proj,
    paste0("cc_conus_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)

### CH regress & update --------------------------------------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image.

ch_reg_raw <- calc_reg_raw("ch")

# Min 0 clamp. MUST happen first before binning
ch_min0 <- terra::clamp(ch_reg_raw, lower = 0, upper = Inf, values = TRUE)

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

# Zero out where existing CC is 0
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

# Based on project parameters, potentially limit to zones_r, removing LF buffer
if (LF_buffer == 0) {
  ch_conus <- terra::ifel(!is.na(zones0km_r), ch_updt, NA)
} else {
  ch_conus <- ch_updt
}

#Save
terra::writeRaster(
  ch_conus,
  file.path(
    folder_out_base,
    version_proj,
    paste0("ch_conus_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
