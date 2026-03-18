# Updates CBH & CBD
# (Note: Runs CONUS-wide, not per zone)

# Only updates disturbed areas
# Uses canopy guide, updated CC and CH,
#      EVT (FVT), DIST, and 
#      Pre-generated disturbance regression table (CBH)

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

#save a copy of only-updated disturbed pixels (does not affect final output)
save_updatedonly <- TRUE

### Data in ------------------------------------------

# from 1_dist_criteria_encoding.R
enc_rast_dist <- rast(file.path(
  folder_out_base,
  version_proj,
  "processing", 
  "encoded_regression_distonly.tif"))


## Regression tables
# (From cloud storage. Created for v1, Carrie/Kyle/Kel?)
cbh_reg_tbl <- read_csv(file.path(
  folder_regtbl, 
  "CBH_Disturbance_Tbl_filled.csv"))


## Raster data for calculations

# Canopy guide
file_cg <- file.path(
  folder_out_base, version_proj, "processing", 
  paste0("canopy_guide_conus_", version_proj, ".tif"))
cg <- rast(file_cg)

# Disturbance
# Set in the project parameters
dist <- rast(dist_file)
#binary version for masking needs
dist_bin <- ifel(dist > 0, 1, NA)


# Existing canopy
file_cbd <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cbd\\.tif$"),
  full.names = TRUE)
cbd_orig <- rast(file_cbd)

file_cbh <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cbh\\.tif$"),
  full.names = TRUE)
cbh_orig <- rast(file_cbh)


# Newly updated CC and CH. 
# We are using the newly generated CC and CH as the midpoint images 
#   instead of FVH/C_Midpoint images
# CC and CH are already binned to midpoint values during their calculation
#  only need to divide CH by 10 to get unscaled midpoint
# Also used in adjusting raw regression results

file_cc_new <- list.files(
  path = file.path(folder_out_base, version_proj), 
  pattern = paste0("^cc_conus_", version_proj, "\\.tif$"),
  full.names = TRUE)
cc_new <- rast(file_cc_new)

file_ch_new <- list.files(
  path = file.path(folder_out_base, version_proj), 
  pattern = paste0("^ch_conus_", version_proj, "\\.tif$"),
  full.names = TRUE)
ch_new <- rast(file_ch_new)
ch_mid_new <- ch_new/10


# UNcoded EVT for CBD-specific regression
# EVT (FVT)
file_fvt_lf <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVT_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
evt_lf <- rast(file_fvt_lf)
# Handle -9999 in original LF raster
evt_lf <- mask(evt_lf, evt_lf, maskvalues=-9999)



# Zones to potentially limit data (i.e. no LF buffer around CONUS)
#  (to match with fbfm zones that were used)
#  LF_buffer set in parameter script file
if (LF_buffer == 0){
  zones0km_r <- rast(file.path(folder_mapzones,
                            "LF_zones_0km_rasterized.tif"))
} 


### CBH regress & update ---------------------------

# Apply the regression equation for the variable
#  and fill in areas not disturbed with original variable image. 
# ~50 min
cbh_reg_raw <- calc_reg_raw(var_layer = "cbh")

## Adjust scale and range
# CBH has precision of one decimal place, 
#  multiplied by 10 -- to be integer value (no decimal)
cbh_10int <- round(cbh_reg_raw*10, digits=0)
# Clamp values between 0 to 100. 
#  (Top value of 100 means CBH >= 10 meters)
cbh_clamp <- clamp(cbh_10int, lower=0, upper=100, values=TRUE)

## Override values
# Zero out where canopy guide is 0 
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbh_cg0 <- mask(cbh_clamp, cg, maskvalues = 0, updatevalue = 0)

# Where canopy guide is 2, manually set value to 100 (10m)
#  per LF TFCT canopy guide description/rules
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbh_cg2 <- mask(cbh_cg0, cg, maskvalues = 2, updatevalue = 100)

# Zero out where NEW CC is 0
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbh_cc0 <- mask(cbh_cg2, cc_new, maskvalues = 0, updatevalue = 0)

# "CBH can't be larger than CH; where it is, reduce CBH to 2/3 of CH"
# (~1 hr)

# There is no reason that the !is.na(cbh_cc0) should be necessary, 
#  and yet it produces results where cbh_cc0 is NA unless it is included.
# This is likely a strange bug in terra::ifel()?? Submitted bug report
# https://github.com/rspatial/terra/issues/2058
# TODO Check back on bug report
# cbh_ch <- ifel(cbh_cc0 > ch_new,
#                round(ch_new * 0.667, digits = 0),
#                cbh_cc0)

cbh_ch <- ifel(!is.na(cbh_cc0) & cbh_cc0 > ch_new,
                  round(ch_new * 0.667, digits = 0),
                  cbh_cc0)

# Limit to ONLY disturbed pixels
cbh_clean <- mask(cbh_ch, dist_bin, maskvalues=1, inverse=TRUE)

# Rename
names(cbh_clean) <- "cbh"
varnames(cbh_clean) <- "cbh"

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not-disturbed-&-regression-calc'd)
cbh_updt <- cover(cbh_clean, cbh_orig, values = NA)

# Based on project parameters, potentially limit to zones_r, removing LF buffer 
if (LF_buffer == 0){
  cbh_conus <- ifel(!is.na(zones0km_r), cbh_updt, NA)
} else {
  cbh_conus <- cbh_updt
}

#Save
writeRaster(cbh_conus,
            file.path(
              folder_out_base, 
              version_proj,
              paste0("cbh_conus_", version_proj, ".tif")),
            gdal=c("COMPRESS=DEFLATE"))


### CBD update ---------------------------

# Does NOT use other canopy regression
#  Uses very particular own regression

#pinion/juniper (FVT) binary 
pj_values <- c(2017, 2019, 2025, 2115, 2116, 2119) 
pj <- ifel(evt_lf %in% pj_values, 1, 0)

# python was: 
#  sh1: "(b('height') < 15) ? 0 : (b('height') < 30) ? 1 : 0"
# Under 15 feet, return 0. Under 30 ft but not under 15 feet, return 1, 
#   otherwise, return 0. 
sh1_rcl <- tribble(
  ~from, ~to, ~becomes,
  -Inf, 15, 0,
  15, 30, 1, 
  30, Inf, 0) %>% 
  as.matrix()
sh1 <- classify(ch_mid_new, rcl = sh1_rcl, right = FALSE)

#  sh2: "(b('height') < 15) ? 0 : (b('height')) < 30 ? 0 : 1"
# Revising to be more succinct >=30 ft, then 1
sh2_rcl <- tribble(
  ~from, ~to, ~becomes,
  -Inf, 30, 0,
  30, Inf, 1) %>% 
  as.matrix()
sh2 <- classify(ch_mid_new, rcl = sh2_rcl, right = FALSE)

# e = exp(1)
rast_e <- init(evt_lf, exp(1))

# # apply CBD equation
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
(start_time <- Sys.time())
cbd_raw <- rast_e^(-2.4887057 + (0.0335917 * cc_new) + 
  (-0.356861 * sh1) + -(0.6006381 * sh2) + 
  (-1.10691 * pj) + (-0.0010804 * (cc_new * sh1)) + 
  (-0.0018324 * (cc_new * sh2)))
(end_time <- Sys.time())
(end_time - start_time)

# This will create values in far more than just disturbed areas. 
# Masking to disturbed here, but will also need to later after overrides. 
cbd_raw_dist <- mask(cbd_raw, dist_bin, maskvalues = 1, inverse = TRUE)

## Adjust scale and range

# LF CBD is in Kg / m^3 * 100
cbd_100 <- cbd_raw_dist * 100

# Clamped to range of 0 to 45 (45 means >= 45)
cbd_clamp <- clamp(cbd_100, lower = 0, upper = 45, values = TRUE)

## Overrides

# Zero out where canopy guide is 0 
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbd_cg0 <- mask(cbd_clamp, cg, maskvalues = 0, updatevalue = 0)

# Where canopy guide is 2, manually set value to 1 (0.012kg/m^3)
#  per LF TFCT canopy guide description/rules
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbd_cg2 <- mask(cbh_cg0, cg, maskvalues = 2, updatevalue = 1)

# Where canopy guide is 3, manually set value to 3 (0.03 kg/m^3)
#  per LF TFCT canopy guide description/rules
# NOTE: This is a change from GEE python where it had been set to 1
#       as like CG=2, but LFTFCT states 0.03 km/m^3 in this case 
cbd_cg3 <- mask(cbh_cg2, cg, maskvalues = 3, updatevalue = 3)

# Zero out where NEW CC is 0
#  (Note that will add extra values outside of disturbed-regressed,
#   so will need to mask to disturbed only later. 
#   Mask is much faster than ifel.)
cbd_cc0 <- mask(cbd_cg3, cc_new, maskvalues = 0, updatevalue = 0)




# Limit to ONLY disturbed pixels
cbd_clean <- mask(cbd_cc0, dist_bin, maskvalues=1, inverse=TRUE)

# Rename
names(cbd_clean) <- "cbd"
varnames(cbd_clean) <- "cbd"

# Fill in with baseline values anywhere where there is
#  no updated values (ie. not-disturbed-&-regression-calc'd)
cbd_updt <- cover(cbd_clean, cbd_orig, values = NA)

# Based on project parameters, potentially limit to zones_r, removing LF buffer 
if (LF_buffer == 0){
  cbd_conus <- ifel(!is.na(zones0km_r), cbd_updt, NA)
} else {
  cbd_conus <- cbd_updt
}

#Save
writeRaster(cbd_conus,
            file.path(
              folder_out_base, 
              version_proj,
              paste0("cbd_conus_", version_proj, ".tif")),
            gdal=c("COMPRESS=DEFLATE"))

