# Script to mosaic/merge the per zone fbfm40 updated rasters

# Run time: ~ 1 hour

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

### Settings -------------------------------------------------------------------

#preventing scientific notation
options(scipen = 999)

### Data in --------------------------------------------------------------------

folder_in <- file.path(
  folder_out_base,
  version_proj,
  "processing",
  "FBFM40_update_zone"
)

if (save_updatedonly) {
  folder_in_uo <- file.path(
    folder_out_base,
    version_proj,
    "processing",
    "FBFM40_zone_distonly_update"
  )
}

# Existing FBFM40.
file_fbfm40 <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fbfm\\.tif$"),
  full.names = TRUE
)
fbfm_orig <- terra::rast(file_fbfm40)

### Mosaic via merge -----------------------------------------------------------

# As the zones were rasterized, and that raster was used to
#  mask the per zone steps, we know there is no funny business
#  with sf crop and ending up with missing or overlapping pixels.
# Can use the simpler and faster merge (but NOT algo 1 which resamples).

files_fbfm <- list.files(folder_in, full.names = TRUE)
if (!length(files_fbfm) == nrow(zones_sf)) {
  stop(paste0(
    "Incorrect number of fbfm per zone rasters. Found ",
    length(files_fbfm),
    ", but",
    nrow(zones_sf),
    "were expected."
  ))
}

#put all into a raster collection (this does not mind different extents)
sprc_fbfm <- do.call(terra::sprc, list(files_fbfm))

# ~50 minutes
#Do not use algo 1 as it resamples. Use algo 3 if a vrt is needed.
conus_fbfm <- terra::merge(sprc_fbfm, algo = 2)

# Fixing extents (had trimmed off edges)
fbfm_full <- terra::extend(conus_fbfm, fbfm_orig)

# Fix raster variable names
varnames(fbfm_full) <- "fbfm40"
names(fbfm_full) <- "fbfm40"

#Save
terra::writeRaster(
  fbfm_full,
  file.path(
    folder_out_base,
    version_proj,
    paste0("fbfm40_conus_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)


if (save_updatedonly) {
  files_fbfm_uo <- list.files(folder_in_uo, full.names = TRUE)
  if (!length(files_fbfm_uo) == nrow(zones_sf)) {
    stop(paste0(
      "Incorrect number of update-only fbfm per zone rasters. Found ",
      length(files_fbfm_uo),
      ", but",
      nrow(zones_sf),
      "were expected."
    ))
  }

  #put all into a raster collection (this does not mind different extents)
  sprc_fbfm_uo <- do.call(terra::sprc, list(files_fbfm_uo))

  #Do not use algo 1 as it resamples. Use algo 3 if a vrt is needed.
  conus_fbfm_uo <- terra::merge(sprc_fbfm_uo, algo = 2)

  # Fixing extents (had trimmed off edges)
  fbfm_full_uo <- terra::extend(conus_fbfm_uo, fbfm_orig)

  # Fix raster variable names
  varnames(fbfm_full) <- "fbfm40"
  names(fbfm_full) <- "fbfm40"

  #Save
  folder_ud <- file.path(folder_out_base, version_proj, "distonly_update")
  dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    fbfm_full_uo,
    file.path(
      folder_ud,
      paste0("fbfm40_updtonly_", version_proj, ".tif")
    ),
    gdal = c("COMPRESS=DEFLATE")
  )
}
