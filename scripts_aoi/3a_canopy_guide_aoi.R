# Creating canopy guide (criteria encoding)
#  for use later in updating CC, CH, CBH, CBD

# Encodes values from DIST, BPS, FVH (coded), FVC (coded), and FVT (coded)
#  into our LFTCT rule encoded value
#  (See scripts in 1_ruleset for generating the encoded values in the crosswalk,
#    here we are creating the encoded values in a raster.)
#  (See 0_landfire_preprocess_aoi.R for encoding of the FVH, FVC, FVT rasters)

# CG = 0 : No canopy (or 9999 rules in later LFTFCT).
#          We will zero out CC/CH/CBH/CBD update values here.
# CG = 1 : We will use the fuel values from the regression in the next steps.
#          (Trees, where torching and crowing is likely.)
# CG = 2 : Using regression update values for CC, CH.
#          CBH is set to 100 (10m) where CG is 2.
#          CBD is set to 1 (~0.012kg/m^3) where CG is 2.
#          (Trees, where torching and crowning not likely.)
# CG = 3 : CBH, CH, CC as regression predicted.
#         (Tree, where torching would occur, not active crowning not likely.)
#         CBD is set to 3 (0.03kg/m^3) where CG is 3.
#           NOTE: Previously set to 1 in GEE python code, updated to 3,
#                 per LFTFCT user guide & canopy rules

#FROM LFLFTFCT Guide:
# 3.4.2 Canopy guide toggle Modeled fire behavior is influenced by many
# vegetation attributes, but in forested areas, canopy is a particularly
# important factor affecting fire behavior outputs.
# A user-defined switch has been incorporated in LFTFC to describe the effects
# of canopy. The switch has the following three numeric settings.
# • 0: This setting indicates no canopy on the site.
# Canopy Base Height (CBH) and Canopy Bulk Density (CBD) calculations from the
# LF data sets would be used to indicate the likelihood of crown fire on the site.
# • 1: This setting would be used on a tree life form site where torching and
# crowning in the upper strata vegetation is common during times of fire activity.
# • 2: This setting would be used on tree life form sites where torching and
# crowning are not likely to occur, such as in some hardwood communities.
# The mapping tool in this setting artificially raises CBH to 10 meters and
# lowers CBD to 0.012 kg/m³. Torching and crowning will not occur but the
# stand attributes (CH and CC) will be used in the calculations of surface fire
# behavior characteristics. The reason for keeping the canopy attributes in  place
# is to adjust wind speed, shading, and sheltering for surface fire calculations.
# • 3: This setting would be used on tree life form sites where torching would
# occur, but active crowning is not likely, such as in mixed hardwood and
# conifer communities. The mapping tool in this setting artificially
# lowers CBD to 0.03 kg/m³, but leaves CBH, CH, and CC as predicted.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

source(file.path("scripts_aoi", "common_variables.R"))
source(file.path("scripts_aoi", "common_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts_aoi", "0_parameters", "dev_trpa_aoi.R"))

#save out encoded pixels that do not match rules, per zone
# QA setting, or change directory in writeRaster call
save_unmatched <- TRUE

### Settings -------------------------------------------------------------------

#preventing scientific notation
options(scipen = 999)

#To avoid issues with large numbers, tell terra to use
#  64 bit numbers, otherwise will see weird rounding on the
#  very large encoded numbers.
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terra::terraOptions(datatype = "FLT8S") #FLT8S

# May not need switch? If have AOI, then use AOI-limited area
#  Use 90km as 'default' for the largest landscape before clip
# # appropriate zone rasters, parameter set in parameter script file
# if (LF_buffer == 0) {
#   folder_zones <- file.path(folder_mapzones, "per_zone_rasters_0km")
# } else if (LF_buffer == 90) {
#   folder_zones <- file.path(folder_mapzones, "per_zone_rasters_90km")
# } else {
#   stop(paste0("Unmatched LF buffer selection: ", LF_buffer))
# }
folder_zones <- file.path(folder_mapzones, "per_zone_rasters_90km")


folder_out <- file.path(
  folder_out_base,
  version_proj,
  "processing",
  "canopy_guide"
)
dir.create(folder_out, recursive = TRUE, showWarnings = FALSE)


### Data in --------------------------------------------------------------------

# LFTFC rules by zone (with neighbors)
#location of LFTFCT ruleset, encoded, with neighboring zones
folder_cmb <- file.path(
  folder_lfrules_base,
  paste0("rules_wneighbors_LF", version_target)
)

# Encoded DIST & criteria raster
coded_dist <- terra::rast(file.path(
  folder_out_base,
  version_proj,
  "processing",
  "encoded_rule_criteria_distonly.tif"
))

# Disturbance
# Set in the project parameters
dist_orig <- terra::rast(dist_file)
dist <- clip_by_aoi(dist_orig, aoi)

### Identify zones -------------------------------------------------------------

# Zones that overlap the AOI, will work per zone below
# all zones (90 km buffer)
#zones90km_sf
# AOI vect to sf
aoi_sf <- aoi %>% sf::st_as_sf()
# which intersects?
# st_intersection() gives warning about assuming spatial constant attribute variables
# suppress that one warning while letting any others through
handle_const_attr <- function(w) {
  condition <- conditionMessage(w)
  if (
    grepl("attribute variables are assumed to be spatially constant", condition)
  ) {
    invokeRestart("muffleWarning")
  }
}
withCallingHandlers(
  aoi_zones_sf <- sf::st_intersection(zones90km_sf, aoi_sf),
  warning = handle_const_attr
)

# list of zone numbers that overlap with AOI
zones_lim <- aoi_zones_sf %>% dplyr::pull(ZONE_NUM)

### Guide by zone --------------------------------------------------------------

# Build function and apply to limited zone list instead.
# include QA flag for intermediate and qa products
# Builds a canopy guide with rules
#  Overlays on top a default raster,
#   where if the pixel was disturbed, then we will use regression value later
#  (canopy guide = 1)

# TODO
# Test build_guide_by_zone() function
# both save_qa and not branches
# lapply function to zones_lim?

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

  # reclassification rules matrix
  this_rcl <- this_rules %>%
    dplyr::select(encoded, NewCanopy) %>%
    as.matrix()

  # read in this zone raster (for masking)
  zone_full <- terra::rast(
    file.path(folder_zones, paste0("z", this_zone_pad, ".tif"))
  )
  zone_r <- clip_by_aoi(zone_full, aoi)

  # crop the disturbed-only criteria raster to this zone extent (not mask here)
  this_coded_dist <- terra::crop(coded_dist, zone_r)

  # mask the criteria (disturbed-only) raster to this zone
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
    dir.create(folder_unmatched_canopy, showWarnings = FALSE, recursive = TRUE)

    #reclassify just the zone (just the disturbed pixels) to the guide values
    # unmatched gets value -1
    this_canopy <- terra::classify(this_enc_d, rcl = this_rcl, others = -1)

    ## Unmatched rules values and counts
    # Get just the unmatched encoded values
    this_err <- terra::mask(
      this_enc_d,
      this_canopy,
      maskvalues = -1,
      inverse = TRUE
    )
    # freq table (too much disk space if saving rasters)
    this_err_freq <- terra::freq(this_err) %>% tibble::as_tibble()
    readr::write_csv(
      this_err_freq,
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

    # Build QA raster
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
    # Start with zone raster as background 1 (undisturbed)
    this_qa_r <- terra::ifel(not.na(zone_r), 1, NA)
    # Update all disturbed pixels, tentatively as value 3
    # (disturbed but no data for criteria encoding)
    this_qa_r <- terra::ifel(this_dist_z > 0, 3, this_qa_r)
    # Update disturbed pixels that HAVE an encoded criteria
    # tentatively as 0 value (disturbed and rule)
    this_qa_r <- terra::ifel(not.na(this_enc_d), 0, this_qa_r)
    # Update to where UNMATCHED rule criteria value
    # capture -1 (unmatched) in this_canopy as QA value flag = 2
    this_qa_r <- terra::ifel(this_canopy == -1, 2, this_qa_r)
    # QA value 4 happens later outside of main QA

    ## end main QA and clean up
    # remove unmatched markers
    # assign them CG value of 1 (use regression calc'd value)
    # (no matching rule but disturbed)
    this_canopy <- terra::mask(
      this_canopy,
      this_canopy,
      maskvalues = -1,
      updatevalue = 1
    )
  } else {
    # Not running QA

    # reclassify just the zone (just the disturbed pixels)
    # unmatched CG value of 1 (use regression calc'd value)
    # (no matching rule but disturbed with encoded criteria value)
    this_canopy <- terra::classify(this_enc_d, rcl = this_rcl, others = 1)
    # FIXME test to make sure works as expected. do NAs propagate as expected?
  } # end if save_qa

  # this_canopy is the rule-matched CG value,
  # with a default of CG = 1 for disturbed pixels without a rule
  # Now we need to catch edge cases of a disturbed pixel without
  #  the data for creating encoded rule value

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
  this_newcanopy <- terra::ifel(
    this_dist %in% high_harvest_list,
    0,
    this_newcanopy
  )

  # Capture this in QA also and save out
  if (save_qa) {
    this_qa_r <- terra::ifel(this_dist %in% high_harvest_list, 4, this_qa_r)
    names(this_qa_r) <- "QA_CG"
    varnames(this_qa_r) <- "QA_CG"
    terra::writeRaster(
      this_qa_r,
      file.path(
        folder_canopy_qa,
        paste0("QA_flag_canopyguide_z", this_zone_pad, ".tif")
      ),
      gdal = c("COMPRESS=DEFLATE")
    )
  }

  #raster names
  names(this_newcanopy) <- "newCanopy"
  varnames(this_newcanopy) <- "newCanopy"

  # Save out
  terra::writeRaster(
    this_newcanopy,
    file.path(folder_out, paste0("canopyguide_z", this_zone_pad, ".tif")),
    gdal = c("COMPRESS=DEFLATE")
  )
}
