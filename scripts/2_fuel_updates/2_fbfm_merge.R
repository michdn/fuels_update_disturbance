# Script to mosaic/merge the per zone fbfm40 updated rasters
### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

### Settings -----------------------------------------

#preventing scientific notation
options(scipen=999)

### Data in --------------------------------------------

folder_in <- file.path(folder_out_base, version_proj, "FBFM40_update_zone")

### Mosaic via merge --------------------------------------

# As the zones were rasterized, and that raster was used to 
#  mask the per zone steps, we know there is no funny business
#  with sf crop and ending up with missing or overlapping pixels.
# Can use the simpler and faster merge (but NOT algo 1 which resamples). 

files_fbfm <- list.files(folder_in, full.names=TRUE)
if (!length(files_fbfm) == nrow(zones_sf)){
  stop(paste0("Incorrect number of fbfm per zone rasters. Found ", 
             length(files_fbfm), ", but", nrow(zones_sf), "were expected."))
}

#put all into a raster collection (this does not mind different extents)
sprc_fbfm <- do.call(sprc, list(files_fbfm))

(start_time <- Sys.time())
# ~50 minutes
#Do not use algo 1 as it resamples. Use algo 3 if a vrt is needed.
conus_fbfm <- merge(sprc_fbfm, algo = 2,
                    filename = file.path(
                      folder_out_base, 
                      version_proj,
                      paste0("fbfm_conus_", version_proj, ".tif")))
(end_time <- Sys.time())
(end_time - start_time)
