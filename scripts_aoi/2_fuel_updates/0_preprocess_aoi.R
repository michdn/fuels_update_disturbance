# Preprocess LANDFIRE data
#  ONLY for specific AOI area, so now saved under project!

# Uses crosswalks created in 1_ruleset/0_encoding_preprocess.R

# 1. Encodes/Reclassifies FVH, FVT, FVC with short encoding ID
#    Also reclassifies -9999 to NA
# 2. Removes -1111, -9999 from FBFM40, canopy variables
# 3. BPS: Removes -9999, recodes -1111 to 8888 (all on raster value)
#         Extends BPS to later LF release extents
# 4. Creates FVC and FVH Midpoint rasters
#     used later in CC and CH calculations

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
source(file.path("scripts_aoi", "preprocessing_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts_aoi", "0_parameters", "dev_trpa_aoi.R"))

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
lapply(
  X = fuel_vars_all,
  FUN = process_fuel,
  folder_xwalk = folder_xwalk,
  folder_lfdata_base = folder_lfdata_base,
  folder_out = folder_out,
  version_target = version_target,
  aoi = aoi
)

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

### Midpoints ------------------------------------------------------------------

# Midpoint calcs
# Calculate FVH and FVC midpoint rasters
#  Later used when updating CC & CH

calc_midpoints(
  fv_var = "FVC",
  folder_lfdata_base = folder_lfdata_base,
  version_target = version_target,
  folder_lut = folder_lut,
  folder_out = folder_out
)
calc_midpoints(
  fv_var = "FVH",
  folder_lfdata_base = folder_lfdata_base,
  version_target = version_target,
  folder_lut = folder_lut,
  folder_out = folder_out
)

## mid point notes

# Tables with the midpoint reclassification values
#  Using the ones that were used in the GEE python code
#  Unknown/no documentation on how they were created (no code, etc.)
#  GCS had versions that were named "LF2020" and without, they seemed the same
#    but code used LF2020 version, so that is what we use below.

#DEVELOPER NOTE
# TODO
# These LUTs are odd to me. They only translate some of the EVC/EVH
#  values to midpoint values, and then leave the rest as the original
#  EVC/EVH value, so the raster ends up a mix of EVC/EVH values and
#  the recoded MidPoint values.
# We seem to be relying on the canopy guide and CC values later to
#  set the regression result update value to 0, for the nonsensical values
#  that would arise from using the original EVC/EVH values in the regression,
#  which is expecting the MidPoint values.
# This seems fragile, but I am hesitant to do much with it on first
#  translation from python GEE to R. On future development, I very much
#  want to come back to this, probably with Carrie, to look at how better
#  to do this, or if it needs to be done this way for some reason.

# DEV NOTE:
# In python GEE, the midpoint collections were named
#  'Midpoint_CC' for FVC and 'Midpoint_CH' for FVH
# (as they are used in updating CC and CH, respectively)
# I kept the name as FVC- and FVH-based, to try and prevent confusion.
