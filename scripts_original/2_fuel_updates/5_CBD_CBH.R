# Updates CBH & CBD
# (Note: Runs CONUS-wide, not per zone)

# Only updates disturbed areas
# Uses canopy guide, updated CC and CH,
#      EVT (FVT), DIST, and
#      Pre-generated disturbance regression table (CBH)

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
cbh_reg_tbl <- readr::read_csv(file.path(
  folder_regtbl,
  "CBH_Disturbance_Tbl_filled.csv"
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

# Existing canopy
file_cbd <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cbd\\.tif$"),
  full.names = TRUE
)
cbd_orig <- terra::rast(file_cbd)

file_cbh <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cbh\\.tif$"),
  full.names = TRUE
)
cbh_orig <- terra::rast(file_cbh)

# Newly updated CC and CH.
# We are using the newly generated CC and CH as the midpoint images
#   instead of FVH/C_Midpoint images
# CC and CH are already binned to midpoint values during their calculation
#  only need to divide CH by 10 to get unscaled midpoint
# Also used in adjusting raw regression results

file_cc_new <- list.files(
  path = file.path(folder_out_base, version_proj),
  pattern = paste0("^cc_conus_", version_proj, "\\.tif$"),
  full.names = TRUE
)
cc_new <- terra::rast(file_cc_new)

file_ch_new <- list.files(
  path = file.path(folder_out_base, version_proj),
  pattern = paste0("^ch_conus_", version_proj, "\\.tif$"),
  full.names = TRUE
)
ch_new <- terra::rast(file_ch_new)
ch_mid_new <- ch_new / 10

# UNcoded EVT for CBD-specific regression
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

# Zones to potentially limit data (i.e. no LF buffer around CONUS)
#  LF_buffer set in parameter script file
if (LF_buffer == 0) {
  zones0km_r <- terra::rast(file.path(
    folder_mapzones,
    "LF_zones_0km_rasterized.tif"
  ))
}

### CBH regress & update -------------------------------------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image.
# ~50 min
cbh_reg_raw <- calc_reg_raw(var_layer = "cbh")

## Adjust scale and range
# CBH has precision of one decimal place,
#  multiplied by 10 -- to be integer value (no decimal)
cbh_10int <- round(cbh_reg_raw * 10, digits = 0)
# Clamp values between 0 to 100.
#  (Top value of 100 means CBH >= 10 meters)
cbh_clamp <- terra::clamp(cbh_10int, lower = 0, upper = 100, values = TRUE)

## Override values
#  (Note that these will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later.
#   Mask is much faster than ifel.)

# Zero out where canopy guide is 0
cbh_cg0 <- terra::mask(cbh_clamp, cg, maskvalues = 0, updatevalue = 0)

# Where canopy guide is 2, manually set value to 100 (10m)
#  per LF TFCT canopy guide description/rules
cbh_cg2 <- terra::mask(cbh_cg0, cg, maskvalues = 2, updatevalue = 100)

# Zero out where NEW CC is 0
cbh_cc0 <- terra::mask(cbh_cg2, cc_new, maskvalues = 0, updatevalue = 0)

# "CBH can't be larger than CH; where it is, reduce CBH to 2/3 of CH"
# (~1 hr)
# Terra 1.9.7 or above: see https://github.com/rspatial/terra/issues/2058
cbh_ch <- terra::ifel(
  cbh_cc0 > ch_new,
  round(ch_new * 0.667, digits = 0),
  cbh_cc0
)

# Limit to ONLY disturbed pixels
cbh_updtdist <- terra::mask(cbh_ch, dist_bin, maskvalues = 1, inverse = TRUE)

# Rename
names(cbh_updtdist) <- "cbh"
varnames(cbh_updtdist) <- "cbh"

if (save_updatedonly) {
  folder_ud <- file.path(folder_out_base, version_proj, "distonly_update")
  dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    cbh_updtdist,
    file.path(
      folder_ud,
      paste0("cbh_updtonly_", version_proj, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not-disturbed-&-regression-calc'd)
cbh_updt <- terra::cover(cbh_updtdist, cbh_orig, values = NA)

# Based on project parameters, potentially limit to zones_r, removing LF buffer
if (LF_buffer == 0) {
  cbh_conus <- terra::ifel(!is.na(zones0km_r), cbh_updt, NA)
} else {
  cbh_conus <- cbh_updt
}

#Save
terra::writeRaster(
  cbh_conus,
  file.path(
    folder_out_base,
    version_proj,
    paste0("cbh_conus_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)


### CBD update -----------------------------------------------------------------

# Does NOT use other canopy regression
#  Uses very particular own regression

#pinion/juniper (FVT) binary
pj_values <- c(2017, 2019, 2025, 2115, 2116, 2119)
pj <- terra::ifel(evt_lf %in% pj_values, 1, 0)

# python was:
#  sh1: "(b('height') < 15) ? 0 : (b('height') < 30) ? 1 : 0"
# Under 15 feet, return 0. Under 30 ft but not under 15 feet, return 1,
#   otherwise, return 0.
sh1_rcl <- tibble::tribble(
  ~from , ~to , ~becomes ,
  -Inf  ,  15 ,        0 ,
     15 ,  30 ,        1 ,
     30 , Inf ,        0
) %>%
  as.matrix()
sh1 <- terra::classify(ch_mid_new, rcl = sh1_rcl, right = FALSE)

#  sh2: "(b('height') < 15) ? 0 : (b('height')) < 30 ? 0 : 1"
# Revising to be more succinct >=30 ft, then 1
sh2_rcl <- tibble::tribble(
  ~from , ~to , ~becomes ,
  -Inf  ,  30 ,        0 ,
     30 , Inf ,        1
) %>%
  as.matrix()
sh2 <- terra::classify(ch_mid_new, rcl = sh2_rcl, right = FALSE)

# e = exp(1)
rast_e <- terra::init(evt_lf, exp(1))

## Apply CBD equation
# ~ 2.3 hours
# python GEE:
# cbd = (
#   ee.Image()
#   .expression(
#     " e ** (-2.4887057 + (0.0335917 * cov) + "
#     "(-0.356861 * sh1) + -(0.6006381 * sh2) + "
#     "(-1.10691 * pj) + (-0.0010804 * (cov * sh1)) "
#     "+ (-0.0018324 * (cov * sh2)))",
#     {
#       "e": ee.Image.constant(math.e),
#       "cov": post_cover_mid_img,
#       "pj": pj,
#       "sh1": sh1,
#       "sh2": sh2,
#     },
#   )
cbd_raw <- rast_e^(-2.4887057 +
  (0.0335917 * cc_new) +
  (-0.356861 * sh1) +
  -(0.6006381 * sh2) +
  (-1.10691 * pj) +
  (-0.0010804 * (cc_new * sh1)) +
  (-0.0018324 * (cc_new * sh2)))

# This will create values CONUS-wide, far more than just disturbed areas.
# Masking to disturbed here, but will also need to later after overrides.
cbd_raw_dist <- terra::mask(cbd_raw, dist_bin, maskvalues = 1, inverse = TRUE)

## Adjust scale and range
# LF CBD is in Kg / m^3 * 100 (integer)
# Clamped to LF range of 0 to 45 (45 means >= 45)
cbd_clamp <- terra::clamp(
  round(cbd_raw_dist * 100, digits = 0),
  lower = 0,
  upper = 45,
  values = TRUE
)

## Overrides
#  (Note that these will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed-only later.
#   Mask is much faster than ifel.)

# Zero out where canopy guide is 0
cbd_cg0 <- terra::mask(cbd_clamp, cg, maskvalues = 0, updatevalue = 0)

# Where canopy guide is 2, manually set value to 1 (0.012kg/m^3)
#  per LF TFCT canopy guide description/rules
cbd_cg2 <- terra::mask(cbd_cg0, cg, maskvalues = 2, updatevalue = 1)

# Where canopy guide is 3, manually set value to 3 (0.03 kg/m^3)
#  per LF TFCT canopy guide description/rules
# NOTE: This is a change from GEE python where it had been set to 1
#       as like CG=2, but LFTFCT states 0.03 km/m^3 in this case
cbd_cg3 <- terra::mask(cbd_cg2, cg, maskvalues = 3, updatevalue = 3)

# Zero out where NEW CC is 0
cbd_cc0 <- terra::mask(cbd_cg3, cc_new, maskvalues = 0, updatevalue = 0)

## Disturbed & update/fill in non-disturbed

# Limit to ONLY disturbed pixels
cbd_updtdist <- terra::mask(cbd_cc0, dist_bin, maskvalues = 1, inverse = TRUE)

# Rename
names(cbd_updtdist) <- "cbd"
varnames(cbd_updtdist) <- "cbd"

if (save_updatedonly) {
  folder_ud <- file.path(folder_out_base, version_proj, "distonly_update")
  dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    cbd_updtdist,
    file.path(
      folder_ud,
      paste0("cbd_updtonly_", version_proj, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not-disturbed-&-regression-calc'd)
cbd_updt <- terra::cover(cbd_updtdist, cbd_orig, values = NA)

# Based on project parameters, potentially limit to zones_r, removing LF buffer
if (LF_buffer == 0) {
  cbd_conus <- terra::ifel(!is.na(zones0km_r), cbd_updt, NA)
} else {
  cbd_conus <- cbd_updt
}

#Save
terra::writeRaster(
  cbd_conus,
  file.path(
    folder_out_base,
    version_proj,
    paste0("cbd_conus_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
