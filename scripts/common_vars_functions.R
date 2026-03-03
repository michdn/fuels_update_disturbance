# Common variables across scripts

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf)

## Folders ----------------------------------

#base folder for LANDFIRE fuels and vegetation data
folder_lfdata_base <- file.path("data", "landfire")

#processed LF data
folder_lfproc <- file.path("data", "landfire_processed")
dir.create(folder_lfproc, recursive = TRUE, showWarnings = FALSE)

#folder for LANDFIRE mapzones
folder_mapzones <- file.path("data", "lf_mapzones")

#base folder for LANDFIRE ruleset data
folder_lfrules_base <- file.path("data", "lf_ruleset")

#folder for FH* crosswalks for encoding criteria
folder_xwalk <- file.path("data", "crosswalks")
dir.create(folder_xwalk, recursive = TRUE, showWarnings = FALSE)

#base folder for output updated fuels and others
folder_out_base <- file.path("data", "output")
dir.create(folder_out_base, recursive = TRUE, showWarnings = FALSE)

## Variables --------------------------------

#map zones
zones_sf <- read_sf(
  file.path("data", "lf_mapzones", 
            "LF_CONUS_mz90k_0k_shps",
            "conus_mz_0k.shp"))

#CONUS zones (Yes, there is no 11. It jumps to 98 and 99.)
#   #c(1:10, 12:66, 98, 99)
zones <- zones_sf %>% pull(ZONE_NUM) %>% sort()


