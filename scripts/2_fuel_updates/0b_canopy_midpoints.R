# Preprocess LANDFIRE data 

# Midpoint calcs
# Calculate FVH and FVC midpoint rasters 
#  Later used when updating CC & CH

#runtime ~45 minutes

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", 
                 "2026_WRME_LF2024_updt2025.R"))

# #preventing scientific notation
# options(scipen=999)



### Data in --------------------------------------------

## Get FVH & FVC rasters -- originals, not encoded versions
file_fvc <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVC_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fvc_orig <- rast(file_fvc)

file_fvh <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVH_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fvh_orig <- rast(file_fvh)


# Tables with the midpoint reclassification values
#  Using the ones that were used in the GEE python code
#  Unknown/no documentation on how they were created (no code, etc.)
#  GCS had versions that were named "LF2020" and without, they seemed the same
#    but code used LF2020 version, so that is what we use below. 

fvc_lut <- read_csv(file.path(
  folder_lfproc, "LUTs", "LFTFCT_tables_LF2020_LUT_Cover.csv"))

fvh_lut <- read_csv(file.path(
  folder_lfproc, "LUTs", "LFTFCT_tables_LF2020_LUT_Height.csv"))

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

### Reclassify / Midpoints ------------------------------------------

# Create reclassification guide
fvc_rcl <- fvc_lut %>% 
  dplyr::select(EVC, MidPoint) %>% 
  as.matrix()

fvh_rcl <- fvh_lut %>% 
  dplyr::select(EVH, MidPoint) %>% 
  as.matrix()


#Reclassify (~30 min)
# -9999 is not explicitly handled, but becomes NA in classifying
fvc_mid <- classify(fvc_orig, rcl = fvc_rcl, others = NA)
names(fvc_mid) <- "FVC_MIDPOINT"
varnames(fvc_mid) <- "FVC_MIDPOINT"

fvh_mid <- classify(fvh_orig, rcl = fvh_rcl, others = NA)
names(fvh_mid) <- "FVH_MIDPOINT"
varnames(fvh_mid) <- "FVH_MIDPOINT"

#Save out
writeRaster(fvc_mid, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fvc_midpoints.tif")),
            gdal=c("COMPRESS=DEFLATE"))
writeRaster(fvh_mid, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fvh_midpoints.tif")),
            gdal=c("COMPRESS=DEFLATE"))

# DEV NOTE:
# In python GEE, the midpoint collections were named
#  'Midpoint_CC' for FVC and 'Midpoint_CH' for FVH 
# (as they are used in updating CC and CH, respectively)
# I kept the name as FVC- and FVH-based, to try and prevent confusion. 
