# Common folders, variables across scripts

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  sf
)

### Folders ----------------------------------

#base folder for LANDFIRE fuels and vegetation data
folder_lfdata_base <- file.path("data", "landfire")

#folder for LANDFIRE mapzones
folder_mapzones <- file.path("data", "lf_mapzones")

#base folder for LANDFIRE ruleset data
folder_lfrules_base <- file.path("data", "lf_ruleset")

#processed LF data
folder_lfproc <- file.path("data", "landfire_processed")
dir.create(folder_lfproc, recursive = TRUE, showWarnings = FALSE)

#folder for FH* crosswalks for encoding criteria
folder_xwalk <- file.path(folder_lfproc, "crosswalks")
dir.create(folder_xwalk, recursive = TRUE, showWarnings = FALSE)

#folder for regression-related reference data
folder_regref <- file.path("data", "regression_reference")
#look up tables for midpoint calculations
folder_lut <- file.path(folder_regref, "LUTs")
#regression tables
folder_regtbl <- file.path(folder_regref, "disturbance_regression")

#base folder for output updated fuels and others
folder_out_base <- file.path("projects")
dir.create(folder_out_base, recursive = TRUE, showWarnings = FALSE)

### Variables --------------------------------

#map zones
zones0km_sf <- sf::read_sf(
  file.path(folder_mapzones, "LF_CONUS_mz90k_0k_shps", "conus_mz_0k.shp")
)

#with 90 km buffer around US
zones90km_sf <- sf::read_sf(
  file.path(folder_mapzones, "LF_CONUS_mz90k_0k_shps", "conus_mz_90k.shp")
)

# CONUS zones (Yes, there is no 11. It jumps to 98 and 99.)
#   #c(1:10, 12:66, 98, 99)
zones <- zones0km_sf %>% dplyr::pull(ZONE_NUM) %>% sort()

### Disturbance variables ------------------------------------------------------

# D_TYPE
# 1: Fire, 2: Mechanical Add, 3: Mechanical Remove, 4: Windthrow,
# 5: Insects-Disease, 6: Mechanical Unknown, 7: Mastication
dtype_xwalk <- tibble::tribble(
  ~DIST_TYPE           , ~D_TYPE ,
  "Fire"               ,       1 ,
  "Wildfire"           ,       1 ,
  "Wildland Fire Use"  ,       1 ,
  "Prescribed Fire"    ,       1 ,
  "Mechanical Add"     ,       2 ,
  "Thinning"           ,       3 , #2->3 correction 2026-10-30
  "Mechanical Remove"  ,       3 ,
  "Clearcut"           ,       3 ,
  "Development"        ,       3 ,
  "Harvest"            ,       3 ,
  "Weather"            ,       4 ,
  "Insects/Disease"    ,       5 ,
  "Insects"            ,       5 ,
  "Disease"            ,       5 ,
  "Mechanical Unknown" ,       6 ,
  "Insecticide"        ,       6 ,
  "Chemical"           ,       6 ,
  "Herbicide"          ,       6 ,
  "Biological"         ,       6 ,
  "Unknown"            ,       6 ,
  "Mastication"        ,       7
)

#D_SEVERITY, 2nd digit
# 1: Low, 2: Moderate, 3: High
dsev_xwalk <- tibble::tribble(
  ~SEVERITY  , ~D_SEVERITY ,
  #"Unburned/Low", "?", #dropping
  "Low"      ,           1 ,
  "Moderate" ,           2 ,
  "High"     ,           3
)
#dropping "No Severity", "Increased Green"

# D_TIME, 3rd digit
# DIST time code rules vary by region
southeast_zones <- c(58, 55, 56, 99, 46)
#fmt:skip
dtime_xwalk_se <- tibble::tibble(
  diff_years = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  D_TIME =     c(1, 1, 2, 3, 3, 3, 3, 3, 3, 3,  3)
)
# Everywhere other than Southeast
# from is years, to is the T time part of DIST code
#fmt:skip
dtime_xwalk <- tibble::tibble(
  diff_years = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
  D_TIME =     c(1, 1, 2, 2, 2, 2, 3, 3, 3, 3,  3)
)

# DIST code priorities when collapsing multiple years
# From Fire Factor fuels:
#  "DIST code ranking priority scheme informed by Landfire analysts"
# Originally only included 1xx, 2xx, 3xx, and 8xx* codes;
#   missing 4xx, 5xx, 6xx, and 7xx codes (added 2026-06-23).
# Decided order would be: 4 -> 7 -> 6 -> 5
# NOTE: ranks revised so that LOWER number ranks have HIGHER priority
# * Perhaps 8xx codes are retired? Kept in as in original, just in case.
# fmt: skip
dist_codes_ordered = c(
  # Fire high sev | most recent to least
  131, 132, 133,               
  # Mech remove high sev | most recent to least
  331, 332, 333,
  # All other fires
  121, 122, 123,                                             
  111, 112, 113,    
  # All other dists ranked by: TSD, type, severity. 
  # Type rank goes in order of:  
  #  moderate and low sev mech remove (3xx), mech add (2xx), other (8xx).
  # Later (2026) added weather (4xx), mastication (7xx),
  #  herbicide/unknown (6xx), and insect/disease (5xx).
  321, 231, 831, 431, 731, 631, 531,                                                     
  311, 221, 821, 421, 721, 621, 521,
  322, 211, 811, 411, 711, 611, 511,                                     
  312, 232, 832, 432, 732, 632, 532,  
  313, 212, 812, 412, 712, 612, 512,  
  233, 833, 433, 733, 633, 533,    
  223, 823, 423, 723, 623, 523,        
  213, 813, 413, 713, 613, 513,
  #no disturbance, to be able to recover afterwards
  0
)
code_to_priority <- tibble::tibble(
  dist_codes = dist_codes_ordered,
  dist_rank = 1:length(dist_codes_ordered)
)
