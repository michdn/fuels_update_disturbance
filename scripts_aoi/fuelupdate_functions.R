### Canopy guide ---------------------------------------------------------------

# Build canopy guide for canopy layer fuel update
#  includes QA flag for intermediate and qa products
# Builds a canopy guide with the rules
#  Overlays on top a default raster,
#   where if the pixel was disturbed, then we will use regression value later
#  (canopy guide = 1)

build_guide_by_zone <- function(
  zone_num,
  version_target,
  coded_dist,
  dist,
  folder_cmb = file.path(
    folder_lfrules_base,
    paste0("rules_wneighbors_LF", version_target)
  ),
  folder_zones = file.path(folder_mapzones, "per_zone_rasters_90km"),
  folder_out = file.path(
    folder_out_base,
    version_proj,
    "processing",
    "canopy_guide"
  ),
  save_qa = FALSE,
  folder_qa = file.path(folder_out_base, version_proj, "qa")
) {
  this_zone_pad <- stringr::str_pad(zone_num, 2, "left", 0)

  # get rules for this zone (plus neighbors)
  this_rules <- readr::read_csv(
    file.path(
      folder_cmb,
      paste0("LF", version_target, "_z", this_zone_pad, "_n_CMB.csv")
    ),
    show_col_types = FALSE
  )

  # Reclassification rules matrix
  # Adding NA to NA pair, so that we can capture unmatched encoded values
  #  during reclass with an others parameter set
  #  (unmatched encoded value = no applicable rule)
  this_rcl <- this_rules %>%
    dplyr::select(encoded, NewCanopy) %>%
    dplyr::bind_rows(tibble::tibble_row(encoded = NA, NewCanopy = NA)) %>%
    as.matrix()

  # read in this zone raster (for masking)
  zone_full <- terra::rast(
    file.path(folder_zones, paste0("z", this_zone_pad, ".tif"))
  )
  # clip the zone raster by the AOI (so zone within aoi extent)
  zone_r <- terra::crop(zone_full, aoi, mask = FALSE)

  # crop the disturbed-only criteria raster to this AOI-zone extent (not mask here)
  this_coded_dist <- terra::crop(coded_dist, zone_r)

  # mask the criteria (disturbed-only) raster to this AOI-zone
  this_enc_d <- terra::mask(
    this_coded_dist,
    zone_r,
    maskvalues = zone_num,
    inverse = TRUE
  )

  # also crop & mask the dist raster
  this_dist_z <- terra::crop(dist, zone_r)
  this_dist_z <- terra::mask(
    this_dist_z,
    zone_r,
    maskvalues = zone_num,
    inverse = TRUE
  )

  # Run QA: catch unmatched rules, create qa raster, freq tables
  if (save_qa) {
    # some do not have rule combinations that exist
    # want to capture info on rules/matching
    folder_canopy_qa <- file.path(folder_qa, "canopy_qa")
    dir.create(folder_canopy_qa, showWarnings = FALSE, recursive = TRUE)

    #reclassify just the zone (just the disturbed pixels) to the guide values
    # unmatched gets value -1
    # Note important to have NA to NA in rcl.
    this_canopy <- terra::classify(this_enc_d, rcl = this_rcl, others = -1)

    ## Unmatched rules values and counts
    # Get just the unmatched encoded values
    this_missing <- terra::mask(
      this_enc_d,
      this_canopy,
      maskvalues = -1,
      inverse = TRUE
    )
    # freq table (too much disk space if saving rasters)
    this_missing_freq <- terra::freq(this_missing) %>% tibble::as_tibble()
    readr::write_csv(
      this_missing_freq,
      file.path(
        folder_canopy_qa,
        paste0("unmatched_encoding_z", this_zone_pad, ".csv")
      )
    )
    # Freq of all canopy guide values
    # with -1s (unmatched) still
    this_canopy_freq <- terra::freq(this_canopy) %>% tibble::as_tibble()
    readr::write_csv(
      this_canopy_freq,
      file.path(
        folder_canopy_qa,
        paste0("guide_results_z", this_zone_pad, ".csv")
      )
    )

    # Start building QA raster
    # # create an image with information of what happened where
    # # QA values:
    # # if disturbed and has rule & canopy value : flag = 0
    # # if not disturbed : flag = 1
    # # if disturbed but no rule (unmatched encoded value) : flag = 2
    # # if disturbed but no data for encoded criteria value : flag = 3
    # # if a special manual case (e.g. high harvest) : flag = 4
    #
    # # Not to be confused with the CANOPY GUIDE VALUES
    #
    # piece 0: disturbed pixels that HAVE an encoded criteria
    qa_0 <- terra::ifel(not.na(this_enc_d), 0, NA)
    # piece 1: zone raster for a background default value of 1 (undisturbed)
    qa_1 <- terra::subst(zone_r, from = zone_num, to = 1)
    # piece 2: UNMATCHED rule criteria value
    # capture -1 (unmatched) in this_canopy as QA value flag = 2
    qa_2 <- terra::ifel(this_canopy == -1, 2, NA)
    # piece 3: all disturbed pixels, TENTATIVELY as value of 3
    #  (disturbed but no criteria encoding)
    qa_3 <- terra::ifel(this_dist_z > 0, 3, NA)

    # Now, cover in priority, most specific to background
    step1 <- terra::cover(qa_2, qa_0) # unmatched on top of dist-encoded rule
    step2 <- terra::cover(step1, qa_3) # on top of all disturbed
    step3 <- terra::cover(step2, qa_1) # on top of background non-disturbed

    # QA value 4 happens later outside of main QA building

    # DEVELOPER NOTE: Hit bug in terra::ifel() and rewrote in
    #  piecemeal and with cover() or SIMPLE ifel() instead.
    # In following command, NAs from this_canopy are inappropriately
    #  propagating through. In terra 1.9.7 and 1.9.50.
    # this_qa_r4 <- terra::ifel(this_canopy == -1, 2, this_qa_r3)

    ## end main QA and clean up
    # remove unmatched markers
    # assign them CG value of 1 (use regression calc'd value)
    # (no matching rule but disturbed)
    this_canopy <- terra::subst(this_canopy, from = -1, to = 1)
  } else {
    # Not running QA

    # reclassify just the zone (just the disturbed pixels)
    # unmatched CG value of 1 (use regression calc'd value)
    # (no matching rule but disturbed with encoded criteria value)
    # Note important to have NA to NA in rcl.
    this_canopy <- terra::classify(this_enc_d, rcl = this_rcl, others = 1)
  } # end if save_qa

  # this_canopy is the rule-matched CG value,
  # with a default of CG = 1 for disturbed pixels without a rule
  # Now we need to catch edge cases of a disturbed pixel without
  #  the data for creating encoded rule value
  # (Have not seen in my test cases, but being paranoid)

  # default of CG = 1 for all disturbed pixels
  #  (whether or not an encoded rule was possible for that pixel)
  cg_dist_default_1 <- terra::ifel(this_dist_z > 0, 1, NA)
  # build newcanopy by laying this_canopy over default
  this_newcanopy <- terra::cover(this_canopy, cg_dist_default_1)
  # Note: In the QA steps, we caught this in qa flag = 3)

  # In areas of high harvest (DIST codes 331, 332, 333), set CG to 0.
  # (i.e. zero out CC/CH/CBH/CBD)
  # This case was handled special as such in the python GEE code.
  high_harvest_list <- c(331, 332, 333)
  cg_harvest <- terra::ifel(this_dist_z %in% high_harvest_list, 0, NA)
  this_newcanopy <- terra::cover(cg_harvest, this_newcanopy)

  # Capture this in QA also and save out
  if (save_qa) {
    qa_4 <- terra::subst(cg_harvest, from = 0, to = 4)
    step4 <- terra::cover(qa_4, step3)

    names(step4) <- "QAflag_CG"
    varnames(step4) <- "QAflag_CG"
    terra::writeRaster(
      step4,
      file.path(
        folder_canopy_qa,
        paste0("QA_flag_canopyguide_z", this_zone_pad, ".tif")
      ),
      gdal = c("COMPRESS=DEFLATE")
    )
  }

  #raster names
  names(this_newcanopy) <- "canopy_guide"
  varnames(this_newcanopy) <- "canopy_guide"

  # Save out
  terra::writeRaster(
    this_newcanopy,
    file.path(folder_out, paste0("canopyguide_z", this_zone_pad, ".tif")),
    gdal = c("COMPRESS=DEFLATE")
  )
}


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

# TODO
get_reg_tbl <- function(var_layer = c("cc", "ch", "cbh")) {
  ## Regression tables
  # (From cloud storage. Created for v1, Carrie/Kyle/Kel?)

  if (var_layer == "cc") {
    this_tbl <- readr::read_csv(
      file.path(
        folder_regtbl,
        "Cover_Disturbance_Tbl.csv"
      ),
      show_col_types = FALSE
    )
  } else if (var_layer == "ch") {
    this_tbl <- height_reg_tbl <- readr::read_csv(
      file.path(
        folder_regtbl,
        "Height_Disturbance_Tbl.csv"
      ),
      show_col_types = FALSE
    )
  } else if (var_layer == "cbh") {
    #TODO update when get to CBH
    this_tbl <- cbh_reg_tbl
  }
}

# get height-related midpoint raster for regression
# for CC & CH, this will be FVH midpoint (created in preprocessing)
# for CBH, this will be the NEW/updated CH layer
get_height_mid <- function(
  for_reg_var = c("cc", "ch", "cbh"),
  folder_lfproc_aoi = file.path(
    folder_out_base,
    version_proj,
    "lf_processed_aoi"
  ),
  version_target
) {
  if (for_reg_var %in% c("cc", "ch")) {
    this_height <- get_preproc_raster(
      lf_proc_var = "fvh_midpoints",
      folder_lfproc_aoi,
      version_target
    )
  } else if (for_reg_var == "cbh") {
    this_height <- get_updated_raster(var = "ch") #FIXME with actual params
  }
}

# get cover-related midpoint raster for regression
# for CC & CH, this will be FVC midpoint (created in preprocessing)
# for CBH, this will be the NEW/updated CC layer
get_cover_mid <- function(
  for_reg_var = c("cc", "ch", "cbh"),
  folder_lfproc_aoi = file.path(
    folder_out_base,
    version_proj,
    "lf_processed_aoi"
  ),
  version_target
) {
  if (for_reg_var %in% c("cc", "ch")) {
    this_cover <- get_preproc_raster(
      lf_proc_var = "fvc_midpoints",
      folder_lfproc_aoi,
      version_target
    )
  } else if (for_reg_var == "cbh") {
    this_cover <- get_updated_raster(var = "cc") #FIXME with actual params
  }
}

# CC, CH, CBH Regression function
# Calculates the regression update values
#  to be adjusted later, per variable
calc_reg_base <- function(
  # TODO add all params that need to pass in for other fxs and calc
  var_layer = c("cc", "ch", "cbh"),
  enc_rast_dist,
  folder_lfproc_aoi,
  version_target
) {
  # TODO May need to set up a check that if running CBH, that CC and CH have been run first?

  # Set up encoding guide
  # This distinguishes CC, CH, and CBH regressions
  this_tbl <- get_reg_tbl(var_layer = var_layer) %>%
    dplyr::mutate(from_code = HDist * 1e4 + EVT_Fill * 1e0)

  #set up reclassification matrices
  rcl_intercept <- make_reg_rcl(reg_tbl = this_tbl, col_name = "intercept")
  rcl_height <- make_reg_rcl(reg_tbl = this_tbl, col_name = "HT_coef")
  rcl_cover <- make_reg_rcl(reg_tbl = this_tbl, col_name = "CC_coef")

  #get rasters of the coefficients for regression
  # NOTE: Regression piece rasters can have fewer pixels than disturbed
  #        (i.e. be a subset), as some DIST-EVT pairs are not in reg tables
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

  # height midpoints
  this_height_mid <- get_height_mid(
    for_reg_var = var_layer,
    folder_lfproc_aoi,
    version_target
  )
  this_cover_mid <- get_cover_mid(
    for_reg_var = var_layer,
    folder_lfproc_aoi,
    version_target
  )

  # for cc and ch:
  # formula: intercept + height_scale * fvh midpoint + cover_scale * fvc midpoint
  # for cbh:
  # formula: intercept + height_scale * new ch + cover_scale * new cc

  # Stack of regression raster & formula pieces
  reg_stack <- c(
    r_intercept,
    r_height_scale * this_height_mid,
    r_cover_scale * this_cover_mid
  )
  # sum to finish the regression
  reg_rast <- terra::app(reg_stack, fun = "sum")
}
