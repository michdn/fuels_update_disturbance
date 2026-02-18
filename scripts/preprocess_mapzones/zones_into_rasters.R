# Preprocessing mapzones
# Into raster (to ensure perfect pixel coverage)
#   So not dealing with crop oddities
# And then dividing into each zone (with different, smaller extents)

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

#need any LF for rasterization
# DEV Note: Create per LF release in case they change size again?
fvh <- rast(
  file.path("data", "landfire", 
            "LF2024_FVH_CONUS", "Tif", "LF2024_FVH_CONUS.tif"))

folder_out <- file.path(folder_mapzones,
                        "per_zone_rasters")
dir.create(folder_out)

# source(file.path("scripts", "0_parameters", 
#                  "2026_WRME_LF2024_updt2025.R"))


### Zones rasterized ------------------------------------------

(start_time <- Sys.time())

zones_r <- rasterize(zones_sf, fvh, field="ZONE_NUM",
                     background = NA, touches = FALSE)
writeRaster(zones_r,
            file.path(folder_mapzones,
                      "LF_zones_rasterized.tif"),
            gdal=c("COMPRESS=DEFLATE"))

# # dev read in
# zones_r <- rast(
#   file.path(folder_mapzones, 
#             "LF_zones_rasterized.tif"))


### Preprocess PER ZONE rasters -------------------------------

for (i in seq_along(zones)){
  
  this_zone_num <- zones[[i]]
  
  #just this zone, trimmed/cropped down
  #7 min
  this_zone_r <- mask(zones_r, zones_r, this_zone_num, inverse = TRUE)
  this_zone_trim <- trim(this_zone_r, padding=1, value=NA)
  
  print(paste0("Zone ", this_zone_num, ". ", 
               i, " of ", length(zones), " at ", Sys.time()))
  
  #saving
  this_zone_pad <- this_zone_num %>% 
    stringr::str_pad(2, "left", pad = "0")
  
  writeRaster(this_zone_trim, 
              file.path(folder_out,
                        paste0("z", this_zone_pad, ".tif")),
              gdal=c(compress="DEFLATE"))
  
}

(end_time <- Sys.time())
(end_time - start_time)
# 14 hours 