# Preprocessing mapzones
# Into raster (to ensure perfect pixel coverage)
#   So not dealing with crop oddities
# And then dividing into each zone (with different, smaller extents)

# 14 hours with 1 version (0km), assuming 28 hours if both run

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

#just need any LF for rasterization
# DEV Note: Create per LF release in case they change size again?
fvh <- rast(
  file.path("data", "landfire", 
            "LF2024_FVH_CONUS", "Tif", "LF2024_FVH_CONUS.tif"))

folder_out_0km <- file.path(folder_mapzones,
                        "per_zone_rasters_0km")
dir.create(folder_out_0km)

folder_out_90km <- file.path(folder_mapzones,
                            "per_zone_rasters_90km")
dir.create(folder_out_90km)


### Zones rasterized ------------------------------------------

(start_time <- Sys.time())

# 0 km version
zones0km_r <- rasterize(zones0km_sf, fvh, field="ZONE_NUM",
                     background = NA, touches = FALSE)
writeRaster(zones0km_r,
            file.path(folder_mapzones,
                      "LF_zones_0km_rasterized.tif"),
            gdal=c("COMPRESS=DEFLATE"))

# 90 km version
zones90km_r <- rasterize(zones90km_sf, fvh, field="ZONE_NUM",
                     background = NA, touches = FALSE)
writeRaster(zones90km_r,
            file.path(folder_mapzones,
                      "LF_zones_90km_rasterized.tif"),
            gdal=c("COMPRESS=DEFLATE"))


# # # dev read in
zones0km_r <- rast(
  file.path(folder_mapzones,
            "LF_zones_0km_rasterized.tif"))
zones90km_r <- rast(
  file.path(folder_mapzones,
            "LF_zones_90km_rasterized.tif"))


### Preprocess PER ZONE rasters -------------------------------

for (i in seq_along(zones)){
  
  this_zone_num <- zones[[i]]
  print(paste0("Zone ", this_zone_num, ". ", 
               i, " of ", length(zones), " at ", Sys.time()))
  #for file names
  this_zone_pad <- this_zone_num %>% stringr::str_pad(2, "left", pad = "0")
  
  ## 0 km
  #just this zone, trimmed/cropped down
  #7 min
  this_zone0km_r <- mask(zones0km_r, zones0km_r, this_zone_num, inverse = TRUE)
  this_zone0km_trim <- trim(this_zone0km_r, padding=0, value=NA)

  #saving
  writeRaster(this_zone0km_trim,
              file.path(folder_out_0km,
                        paste0("z", this_zone_pad, ".tif")),
              gdal=c(compress="DEFLATE"),
              overwrite = TRUE)
  
  ## 90 km 
  #just this zone, trimmed/cropped down
  #7 min
  this_zone90km_r <- mask(zones90km_r, zones90km_r, this_zone_num, inverse = TRUE)
  #for 90km, MUST have no padding, as edges go right to edge.
  this_zone90km_trim <- trim(this_zone90km_r, padding=0, value=NA)
  
  #saving
  writeRaster(this_zone90km_trim, 
              file.path(folder_out_90km,
                        paste0("z", this_zone_pad, ".tif")),
              gdal=c(compress="DEFLATE"),
              overwrite = TRUE)
  
}

(end_time <- Sys.time())
(end_time - start_time)
