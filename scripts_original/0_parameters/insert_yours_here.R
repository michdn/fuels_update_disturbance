# Parameter file for a specific run

## Version info ----------------------------------------------------------------

## Project
# Your version for this project
version_proj <- "my_project_name_v1"

## LANDFIRE fuels and ruleset
#  For automatic selection of files
#  This is the LANDFIRE version, so LF 2024 is "2024"
#  This is the base LANDFIRE fuel and vegetation data, and LFTFCT
version_target <- "2024"

## LANDFIRE BPS
# BPS only exists for some releases
# This is the latest/version you want to use of BPS
version_bps <- "2020"

## Use 0km or 90km buffer for LF zones/data
# c(0, 90)
LF_buffer <- 90

## Disturbance info ------------------------------------------------------------

# List of LF disturbance rasters to use in creating DIST code raster
#  Only one for a single year update, or multiple for multiple year update.
# MUST BE Landfire disturbance rasters, in their LF folder structure,
#  as code will use their standard format for pulling year from file name,
#  and using LF disturbance codes to DIST crosswalks
lf_dists <- c(
  file.path(
    "data",
    "landfire",
    "LF2025_Dist25_CONUS",
    "Tif",
    "LF2025_Dist25_CONUS.tif"
  )
)

## DIST raster location
# This could be created from LF disturbance data only,
#  or and/or burn severity estimation of very recent fires
#  or anywhere else that would produce DIST codes
# MUST BE in the same projection, and have the same extent and origin
#  as the LF version_target above.
dist_file <- file.path(
  ""
)
