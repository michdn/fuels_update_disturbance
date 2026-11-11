# Preprocess LANDFIRE data
#  ONLY for specific AOI area, so now saved under project!

# 1. Encodes/Reclassifies FVH, FVT, FVC with short encoding ID
#    Also reclassifies -9999 to NA
# 2. Removes -1111, -9999 from FBFM40, canopy variables
# 3. BPS: Removes -9999, recodes -1111 to 8888 (all on raster value)
#         Extends BPS to later LF release extents

# Uses crosswalks created in 1_ruleset/0_encoding_preprocess.R

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  tidyselect,
  terra
)

source(file.path("scripts_aoi", "common_variables.R"))
source(file.path("scripts_aoi", "common_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts_aoi", "0_parameters", "dev_west2026_trpa_aoi.R"))

# project folder for prep
folder_out <- file.path(folder_out_base, version_proj, "lf_processed_aoi")
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)

### FV{C|H|T} variables --------------------------------------------------------

fv_vars_all <- c("FVC", "FVH", "FVT")
lapply(
  X = fv_vars_all,
  FUN = process_fv,
  folder_xwalk = folder_xwalk,
  folder_lfdata_base = folder_lfdata_base,
  folder_out = folder_out,
  version_target = version_target,
  aoi = aoi
)

## Fuels ----------------------------------------------------------------

#process each
fuel_vars_all <- c("FBFM40", "CC", "CH", "CBD", "CBH")
(start_time <- Sys.time())
lapply(
  X = fuel_vars_all,
  FUN = process_fuel,
  folder_xwalk = folder_xwalk,
  folder_lfdata_base = folder_lfdata_base,
  folder_out = folder_out,
  version_target = version_target,
  aoi = aoi
)
(end_time <- Sys.time())
(end_time - start_time)

## BPS LF 2020 ----------------------------------------------------------------

# Note: we are using the Raster values (in rules too)
# R will default and perhaps show BPS_MODEL instead but math is on raster value
process_bps(
  folder_xwalk,
  folder_lfdata_base,
  folder_out,
  version_bps,
  aoi
)
