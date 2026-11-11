# Parameter file for a specific run

## Version info ----------------------------------------------------------------

## Project
# Your version for this project
version_proj <- "west2026_trpa_aoi"

## LANDFIRE fuels and ruleset
#  For automatic selection of files
#  This is the LANDFIRE version, so LF 2024 is "2024"
#  This is the base LANDFIRE fuel and vegetation data, and LFTFCT
# NOTE : for 2026-10 run, using copy of 2024 LFTFCT as if 2025!
version_target <- "2025"

## LANDFIRE BPS
# BPS only exists for some releases
# This is the latest/version you want to use of BPS
version_bps <- "2020"

## Disturbance info ------------------------------------------------------------

# List of LF disturbance rasters to use in creating DIST code raster
#  Only one for a single year update, or multiple for multiple year update.
# MUST BE Landfire disturbance rasters, in their LF folder structure,
#  as code will use their standard format for pulling year from file name,
#  and using LF disturbance codes to DIST crosswalks
lf_dists <- c(
  file.path(
    ""
  )
)
# Not used in this dev run.

## DIST raster location
# This could be created from LF disturbance data only,
#  or and/or burn severity estimation of very recent fires
#  or anywhere else that would produce DIST codes
# MUST BE in the same projection, and have the same extent and origin
#  as the LF version_target above.
dist_file <- file.path(
  "projects",
  "pc660_2026_west",
  "disturbance",
  "DIST_burnseverity_Oct2025_Sep2026.tif"
)
# Borrowing from other project

### AOI ------------------------------------------------------------------------

## shapefile of AOI
# as terra::vect() for use in terra::crop()
# in EPSG:5070 (or if expand to any fuel input, whatever CRS of the fuel rasters)
aoi <- terra::vect(file.path(
  folder_out_base,
  version_proj,
  "aoi",
  "TRPA_aoi_50km.shp"
))

# TODO Probably no longer relevant in AOI scripts?
#  also want some kind of check that extent of AOI doesn't go beyond fuels...
## Use 0km or 90km buffer for LF zones/data
# c(0, 90)
LF_buffer <- 90
