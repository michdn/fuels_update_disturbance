# FM40 update
# Loops by zone
# Writes out each zone

# 1.5-2 hrs for all zone loop

# See next script to mosaic

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  terra
)

source(file.path("scripts", "common_vars_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

#save out encoded pixels that do not match rules, per zone
save_unmatched <- FALSE
#save a copy of only-updated disturbed pixels (does not affect final output)
save_updatedonly <- TRUE

### Settings -------------------------------------------------------------------

#preventing scientific notation
options(scipen = 999)

#To avoid issues with large numbers, tell terra to use
#  64 bit numbers, otherwise will see rounding on the
#  very large encoded numbers.
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terra::terraOptions(datatype = "FLT8S") #FLT8S

# appropriate zone rasters, parameter set in parameter script file
if (LF_buffer == 0) {
  folder_zones <- file.path(folder_mapzones, "per_zone_rasters_0km")
} else if (LF_buffer == 90) {
  folder_zones <- file.path(folder_mapzones, "per_zone_rasters_90km")
} else {
  stop(paste0("Unmatched LF buffer selection: ", LF_buffer))
}

### Data in --------------------------------------------------------------------

# Encoded DIST & criteria raster
coded_dist <- terra::rast(file.path(
  folder_out_base,
  version_proj,
  "processing",
  "encoded_rule_criteria_distonly.tif"
))

# Existing FBFM40
file_fbfm40 <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fbfm\\.tif$"),
  full.names = TRUE
)
fbfm <- terra::rast(file_fbfm40)

### Update by zone -------------------------------------------------

for (i in seq_along(zones)) {
  this_zone_num <- zones[[i]]
  this_zone_pad <- stringr::str_pad(this_zone_num, 2, "left", 0)

  print(paste0(
    "Starting zone ",
    this_zone_pad,
    ". Zone ",
    i,
    " of ",
    length(zones),
    " at ",
    Sys.time()
  ))

  #get rules for this zone (plus neighbors)
  this_rules <- readr::read_csv(
    file.path(
      folder_lfrules_base,
      paste0("rules_wneighbors_LF", version_target),
      paste0("LF", version_target, "_z", this_zone_pad, "_n_CMB.csv")
    ),
    show_col_types = FALSE
  )

  #reclassification rules matrix
  this_rcl <- this_rules %>%
    dplyr::select(encoded, NewFBFM40) %>%
    as.matrix()

  #read in this zone raster (for masking)
  this_z_r <- terra::rast(
    file.path(folder_zones, paste0("z", this_zone_pad, ".tif"))
  )

  #crop the disturbed-only critera raster to this zone extent (not mask here)
  # <1 min
  this_coded_dist <- terra::crop(coded_dist, this_z_r)

  #mask the criteria (disturbed-only) raster to this zone
  # a few secs
  this_enc_d <- terra::mask(
    this_coded_dist,
    this_z_r,
    maskvalues = this_zone_num,
    inverse = TRUE
  )

  #reclassify just the zone (just the disturbed pixels)
  #3-4 min
  this_updt <- terra::classify(this_enc_d, rcl = this_rcl, others = -1)

  #some do not have rule combinations that exist
  # (zone 5 test case - none existed in all CONUS rules)
  # save out here if wanted
  if (save_unmatched) {
    folder_qa <- file.path("data", "test_data", "unmatched_rules")
    dir.create(folder_qa, showWarnings = FALSE, recursive = TRUE)

    this_err <- terra::mask(
      this_enc_d,
      this_updt,
      maskvalues = -1,
      inverse = TRUE
    )

    terra::writeRaster(
      this_err,
      file.path(
        folder_qa,
        paste0("unmatched_encoding_z", this_zone_pad, ".tif")
      ),
      gdal = c("COMPRESS=DEFLATE"),
      datatype = "FLT8S",
      overwrite = TRUE
    )
  } # end if save_unmatched

  #remove unmatched markers
  this_updt <- terra::mask(this_updt, this_updt, maskvalues = -1)

  #raster names
  names(this_updt) <- "new_fbfm40"
  varnames(this_updt) <- "new_fbfm40"

  if (save_updatedonly) {
    folder_ud <- file.path(
      folder_out_base,
      version_proj,
      "processing",
      "FBFM40_zone_distonly_update"
    )
    dir.create(folder_ud, showWarnings = FALSE, recursive = TRUE)

    terra::writeRaster(
      this_updt,
      file.path(folder_ud, paste0("fbfm40_z", this_zone_pad, "_updtonly.tif")),
      gdal = c("COMPRESS=DEFLATE"),
      #python .int16() #changed from .uint16() per request of Chris L. for 2024 run
      # R terra equivalent is INT2S.
      datatype = "INT2S"
    )
  } # end if save_unmatched

  ## Zone update and cover with baseline FBFM40 here, CONUS mosaic afterwards
  ## Add in baseline FBFM40
  #crop baseline fbfm to zone extent
  this_fbfm_crop <- terra::crop(fbfm, this_z_r)
  #and mask it to just the zone
  # this prevents non-zone pixels from ending up in the cover()
  # and means that the zone tifs will not overlap
  this_fbfm <- terra::mask(
    this_fbfm_crop,
    this_z_r,
    maskvalues = this_zone_num,
    inverse = TRUE
  )
  #fill in with baseline values anywhere inside the zone
  #  where there is no updated values (ie. not disturbed)
  this_updt_fbfm <- terra::cover(this_updt, this_fbfm, values = NA)

  # Save out
  folder_out <- file.path(
    folder_out_base,
    version_proj,
    "processing",
    "FBFM40_update_zone"
  )
  dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)

  terra::writeRaster(
    this_updt_fbfm,
    file.path(folder_out, paste0("fbfm40_z", this_zone_pad, ".tif")),
    gdal = c("COMPRESS=DEFLATE"),
    #python .int16() #changed from .uint16() per request of Chris L. for 2024 run
    # R terra equivalent is INT2S.
    datatype = "INT2S"
  )

  # TODO
  #QA flags
  # 1: disturbed and UPDATED value
  # 2: disturbed but SAME value as before
  # merge with -1 unmatched rules above??
} # end per zone (i)
