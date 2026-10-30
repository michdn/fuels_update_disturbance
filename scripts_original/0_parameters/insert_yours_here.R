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

## DIST raster location
# This could be created from LF disturbance data only,
#  or and/or burn severity estimation of very recent fires
#  or anywhere else that would produce DIST codes
# MUST BE in the same projection, and have the same extent and origin
#  as the LF version_target above.
dist_file <- file.path(
  ""
)

## Use 0km or 90km buffer for LF zones/data
# c(0, 90)
LF_buffer <- 90
