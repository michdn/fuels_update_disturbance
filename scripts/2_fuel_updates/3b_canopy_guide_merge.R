# Script to mosaic/merge the per zone canopy guide rasters

# Run time: ~ 1 hour

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

folder_in <- file.path(folder_out_base, version_proj, 
                       "processing", "canopy_guide")

#Using CC to match extents
file_cc <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_cc\\.tif$"),
  full.names = TRUE)
cc_orig <- rast(file_cc)

### Mosaic via merge --------------------------------------

# As the zones were rasterized, and that raster was used to 
#  mask the per zone steps, we know there is no funny business
#  with sf crop and ending up with missing or overlapping pixels.
# Can use the simpler and faster merge (but NOT algo 1 which resamples). 
# Will have to fix outer extents (it shaved off edges). 

files_cg <- list.files(folder_in, full.names=TRUE)
if (!length(files_cg) == nrow(zones_sf)){
  stop(paste0("Incorrect number of canopy guide per zone rasters. Found ", 
             length(files_cg), ", but", nrow(zones_sf), "were expected."))
}

#put all into a raster collection (this does not mind different extents)
sprc_cg <- do.call(sprc, list(files_cg))

(start_time <- Sys.time())
# ~50 minutes
#Do not use algo 1 as it resamples. Use algo 3 if a vrt is needed.
conus_cg <- merge(sprc_cg, algo = 2)

# Fixing extents (had trimmed off edges)
cg_full <- extend(conus_cg, cc_orig)
varnames(cg_full) <- "newCanopy"

#Save
writeRaster(cg_full,
            file.path(
              folder_out_base, 
              version_proj,
              "processing",
              paste0("canopy_guide_conus_", version_proj, ".tif")),
            gdal=c("COMPRESS=DEFLATE"))

(end_time <- Sys.time())
(end_time - start_time)
