# Mosaic of GEE burn severity tiles

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

### Settings -------------------------------------------------------------------

#should be project-level folder. from params? TODO
folder_out <- file.path("projects", "wrme2025_v3", "burn_severity_fromGEE")


# Reclassing values:
# From burn severity code:
# '''generates miller thresholds from 1-4
# 1 : #very low or unburned
# 2 : low
# 3 : moderate
# 4 : high
# ---- in FF fuels:
# dist_remap_from = ee.List([1,2,3,4]) # BS classes output from burn-severity-gee tool [very low/unburned,low,mod,high]
# dist_remap_to = ee.List([0,111,121,131])

bs_xwalk <- tibble::tribble(
  ~gee_bs , ~DIST ,
        0 , NA    , #gee background export value
        1 , NA    ,
        2 ,   111 ,
        3 ,   121 ,
        4 ,   131
)

bs_rcl <- bs_xwalk %>%
  as.matrix()

### Data -----------------------------------------------------------------------

#folder where tiles live
folder_tiles <- file.path(
  "projects",
  "wrme2025_v3",
  "burn_severity_fromGEE",
  "GEE_output_bsgee_v1"
)

files_tifs <- list.files(
  path = folder_tiles,
  pattern = "\\.tif$",
  full.names = TRUE
)

### Checking min & max ---------------------------------------------------------
# Too little data for QGIS to auto pick up on.
# Default min/maxes showed no data. Had to classify to show values
#  that were identified as existing with code below.

# # for each of the tiles, check the min and max
# collector_range <- vector("list", length(files_tifs))

# for (i in seq_along(files_tifs)) {
#   this_file <- files_tifs[[i]]
#   this_filename <- basename(this_file)

#   this_rast <- terra::rast(this_file)
#   collector_range[[i]] <- terra::global(this_rast, c("range")) %>%
#     tibble::as_tibble() %>%
#     dplyr::mutate(file_source = this_filename)
# }

# all_ranges <- bind_rows(collector_range)

# all_ranges %>% arrange(desc(min))
# all_ranges %>% arrange(desc(max))

### Mosaic / merge -------------------------------------------------------------

# We can mosaic using the faster merge with algo=2
#  Tiles overlap, but values in overlap portion are the same in both tiles
#  Merge will take first (default) value, fine.

tile_sprc <- terra::sprc(files_tifs)

(start_time <- Sys.time())
# 45 minutes
bs_mos <- terra::merge(tile_sprc, algo = 2)
(end_time <- Sys.time())
(end_time - start_time)

# 7 min
bs_dist <- terra::classify(bs_mos, rcl = bs_rcl)
names(bs_dist) <- "DIST"

# terra::freq(bs_dist) %>% tibble::as_tibble()

terra::writeRaster(
  bs_dist,
  #dynamic file name? #TODO
  file.path(folder_out, "burnseverity_dist_coded.tif"),
  gdal = c("COMPRESS=DEFLATE"),
  overwrite = TRUE
)
