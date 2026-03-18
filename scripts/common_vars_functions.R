# Common folders, variables, functions across scripts

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  sf)

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
folder_out_base <- file.path("data", "output")
dir.create(folder_out_base, recursive = TRUE, showWarnings = FALSE)

### Variables --------------------------------

#map zones
zones0km_sf <- read_sf(
  file.path(folder_mapzones, 
            "LF_CONUS_mz90k_0k_shps",
            "conus_mz_0k.shp"))

#with 90 km buffer around US
zones90km_sf <- read_sf(
  file.path(folder_mapzones, 
            "LF_CONUS_mz90k_0k_shps",
            "conus_mz_90k.shp"))

#CONUS zones (Yes, there is no 11. It jumps to 98 and 99.)
#   #c(1:10, 12:66, 98, 99)
zones <- zones0km_sf %>% pull(ZONE_NUM) %>% sort()

### Functions ---------------------------------

#Helper function
make_reg_rcl <- function(reg_tbl, col_name){
  reg_tbl %>% 
    dplyr::select(from_code, col_name) %>% 
    #very important to reduce rcl length for reclassifying time
    distinct() %>% 
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
calc_reg_raw <- function(var_layer = c("cc", "ch", "cbh")){
  
  if (var_layer == "cc"){
    
    this_tbl <- cover_reg_tbl
    
  } else if (var_layer == "ch"){
    
    this_tbl <- height_reg_tbl
    
  } else if (var_layer == "cbh"){
    
    this_tbl <- cbh_reg_tbl
    
  }
  
  #set up encoding guide
  this_tbl <- this_tbl %>% 
    mutate(from_code = HDist * 1e4 + EVT_Fill * 1e0)
  
  #set up reclassification matrices
  rcl_intercept <- make_reg_rcl(reg_tbl = this_tbl, col_name = "intercept")
  rcl_height <- make_reg_rcl(reg_tbl = this_tbl, col_name = "HT_coef")
  rcl_cover <- make_reg_rcl(reg_tbl = this_tbl, col_name = "CC_coef")
  
  #get rasters of the coefficients for regression
  r_intercept <- classify(enc_rast_dist, rcl=rcl_intercept, others=NA)
  r_height_scale <- classify(enc_rast_dist, rcl=rcl_height, others=NA)
  r_cover_scale <- classify(enc_rast_dist, rcl=rcl_cover, others=NA)
  
  # NOTE: Regression piece rasters can have fewer pixels than disturbed 
  #        (i.e. be a subset), as some DIST-EVT pairs are not in reg tables
  
  # Regress stack and calc
  if (var_layer %in% c("cc", "ch")){
    # formula: intercept + height_scale * fvh midpoint + cover_scale * fvc midpoint
    (start_time_rr <- Sys.time())
    reg_stack <- c(r_intercept,
                   r_height_scale * height_mid,
                   r_cover_scale * cover_mid)
    reg_rast <- terra::app(reg_stack, fun = "sum")
    (end_time_rr <- Sys.time())
    (end_time_rr - start_time_rr)
    
  } else if (var_layer == "cbh"){
    
    # formula: intercept + height_scale * new ch + cover_scale * new cc
    (start_time_rr <- Sys.time())
    reg_stack <- c(r_intercept,
                   r_height_scale * ch_mid_new,
                   r_cover_scale * cc_new)
    reg_rast <- terra::app(reg_stack, fun = "sum")
    (end_time_rr <- Sys.time())
    (end_time_rr - start_time_rr)
    
  }
  
  return(reg_rast)
  
}
