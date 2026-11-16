### Regression-related ---------------------------------------------------------

#Helper function for regression reclassification matrix
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
