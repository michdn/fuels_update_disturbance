# Common folders, variables, functions across scripts

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

### Functions ---------------------------------

#Helper function
make_reg_rcl <- function(reg_tbl, col_name) {
  reg_tbl %>%
    #using all_of() because of external vector in selection
    #  (col_name is a variable, with the value of the column name)
    dplyr::select(from_code, tidyselect::all_of(col_name)) %>%
    #very important to reduce rcl length for reclassifying time
    dplyr::distinct() %>%
    as.matrix()
}

#CBH Regression function
# Calculates the regression update values
#  to be adjusted later, per variable
# TODO Dev - references tables read in the 4_CC_CH.R & 5_CBD_CBH.R scripts
#      Dev - calc rasters include
#            All: r_intercept, r_height_scale, r_cover_scale
#                 (calc'd from enc_rast_dist and tables above)
#            CC/CH: height_mid, cover_mid
#            CBH: new CH, new CC
calc_reg_raw <- function(var_layer = c("cc", "ch", "cbh")) {
  if (var_layer == "cc") {
    this_tbl <- cover_reg_tbl
  } else if (var_layer == "ch") {
    this_tbl <- height_reg_tbl
  } else if (var_layer == "cbh") {
    this_tbl <- cbh_reg_tbl
  }

  #set up encoding guide
  this_tbl <- this_tbl %>%
    dplyr::mutate(from_code = HDist * 1e4 + EVT_Fill * 1e0)

  #set up reclassification matrices
  rcl_intercept <- make_reg_rcl(reg_tbl = this_tbl, col_name = "intercept")
  rcl_height <- make_reg_rcl(reg_tbl = this_tbl, col_name = "HT_coef")
  rcl_cover <- make_reg_rcl(reg_tbl = this_tbl, col_name = "CC_coef")

  #get rasters of the coefficients for regression
  r_intercept <- terra::classify(
    enc_rast_dist,
    rcl = rcl_intercept,
    others = NA
  )
  r_height_scale <- terra::classify(
    enc_rast_dist,
    rcl = rcl_height,
    others = NA
  )
  r_cover_scale <- terra::classify(enc_rast_dist, rcl = rcl_cover, others = NA)

  # NOTE: Regression piece rasters can have fewer pixels than disturbed
  #        (i.e. be a subset), as some DIST-EVT pairs are not in reg tables

  # Regress stack and calc
  if (var_layer %in% c("cc", "ch")) {
    # formula: intercept + height_scale * fvh midpoint + cover_scale * fvc midpoint
    reg_stack <- c(
      r_intercept,
      r_height_scale * height_mid,
      r_cover_scale * cover_mid
    )
    reg_rast <- terra::app(reg_stack, fun = "sum")
  } else if (var_layer == "cbh") {
    # formula: intercept + height_scale * new ch + cover_scale * new cc
    (start_time_rr <- Sys.time())
    reg_stack <- c(
      r_intercept,
      r_height_scale * ch_mid_new,
      r_cover_scale * cc_new
    )
    reg_rast <- terra::app(reg_stack, fun = "sum")
    (end_time_rr <- Sys.time())
    (end_time_rr - start_time_rr)
  }

  return(reg_rast)
}
