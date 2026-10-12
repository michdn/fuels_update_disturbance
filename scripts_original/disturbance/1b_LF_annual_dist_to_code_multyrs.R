# LF annual disturbance
# Creating DIST codes from LF disturbance data
# This version handles MULTIPLE years, given files in a list in the settings

# Note: Codes changed in LF2024, so creating DYNAMIC crosswalks
# Note2: Naming scheme changed in some places (per year Annual Disturbance)
#        but not in others (All Years Annual Disturbance).
#        Code below assumes the per year download scheme.

# If this creates the final disturbance, update the file pointer in
#  the project parameter R script
# This is a MANUAL change.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  stringr,
  terra
)

# also contains all disturbance crosswalks
source(file.path("scripts_original", "common_vars_functions.R"))

### User settings --------------------------------------------------------------

# project settings
source(file.path(
  "scripts_original",
  "0_parameters",
  "2026_pc666_garnetLF_v2.R"
)) # an important setting from above is: lf_dists

# what year are we updating to?
# this will be used in calculations for how long since disturbance.
target_year <- 2026

# save per year rasters (for multi-year runs), in addition to
#  final combined raster
save_per_year <- TRUE

# out folder, edit with caution
folder_out <- file.path(folder_out_base, version_proj, "disturbance")
dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)

### Functions ------------------------------------------------------------------

get_dist_year <- function(dist_filepath) {
  this_basename <- basename(dist_filepath)
  lf_year <- stringr::str_split_i(this_basename, "_", 1)
  dist_year <- gsub("LF", "", lf_year)
}
get_dist_release <- function(dist_filepath) {
  this_basename <- basename(dist_filepath)
  dist_release <- stringr::str_split_i(this_basename, "_", 2)
}

find_dist_csv <- function(dist_filepath) {
  dist_year <- get_dist_year(dist_filepath)
  dist_release <- get_dist_release(dist_filepath)

  #up one folders (out of Tif folder)
  dist_folder_base <- dirname(dirname(dist_filepath))

  file_dist_csv <- file.path(
    dist_folder_base,
    "CSV_Data",
    paste0("LF", dist_year, "_", dist_release, ".csv")
  )
  dist_csv <- readr::read_csv(file_dist_csv, show_col_types = FALSE)
}

set_up_rcl <- function(
  dist_csv,
  region = c("nonsoutheast", "southeast"),
  yr_diff
) {
  # uses crosswalks in common vars

  # First, get special codes that will go in both final crosswalks
  # Get NoData (-9999), NA (0), and Water mask value (sometimes 2001)
  dist_special <- dist_csv %>%
    dplyr::filter(DIST_TYPE %in% c("Fill-NoData")) %>%
    dplyr::mutate(DIST = NA_real_) %>%
    dplyr::rows_append(
      dist_csv %>%
        dplyr::filter(DIST_TYPE %in% c(NA, "Water")) %>%
        dplyr::mutate(DIST = 0)
    ) %>%
    dplyr::select(VALUE, DIST_TYPE, SEVERITY, DIST)

  if (region == "southeast") {
    xwalk_DIST <- dplyr::bind_rows(
      dist_special,
      dist_csv %>%
        dplyr::select(VALUE, DIST_TYPE, SEVERITY) %>%
        #adds year difference from LF version and disturbance version
        dplyr::mutate(diff_years = .env$yr_diff) %>%
        #join all xwalks, dropping those unmatched (don't want NAs)
        dplyr::inner_join(dtype_xwalk, by = dplyr::join_by(DIST_TYPE)) %>%
        dplyr::inner_join(dsev_xwalk, by = dplyr::join_by(SEVERITY)) %>%
        # SOUTHEAST version
        dplyr::inner_join(dtime_xwalk_se, by = join_by(diff_years)) %>%
        # make DIST code
        dplyr::mutate(
          DIST = paste0(D_TYPE, D_SEVERITY, D_TIME) %>% as.numeric()
        )
    )
  } else if (region == "nonsoutheast") {
    xwalk_DIST <- dplyr::bind_rows(
      dist_special,
      dist_csv %>%
        dplyr::select(VALUE, DIST_TYPE, SEVERITY) %>%
        #adds year difference from LF version and disturbance version
        dplyr::mutate(diff_years = .env$yr_diff) %>%
        #join all xwalks, dropping those unmatched (don't want NAs)
        dplyr::inner_join(dtype_xwalk, by = dplyr::join_by(DIST_TYPE)) %>%
        dplyr::inner_join(dsev_xwalk, by = dplyr::join_by(SEVERITY)) %>%
        dplyr::inner_join(dtime_xwalk, by = join_by(diff_years)) %>%
        # make DIST code
        dplyr::mutate(
          DIST = paste0(D_TYPE, D_SEVERITY, D_TIME) %>% as.numeric()
        )
    )
  }
  #rcl needed for classify()
  rcl_DIST <- xwalk_DIST %>%
    dplyr::select(VALUE, DIST) %>%
    as.matrix()

  # TODO
  # NOTE: This will silently drop things that aren't in mini crosswalks above
  #  Probably want to build out something more robust or write out unmatched to
  #   review. Some are intentionally dropped (low/unburned, etc.)
}

### Data -----------------------------------------------------------------------

# Also need the zones for the different D-TIME crosswalks
# appropriate zone rasters, parameter set in parameter script file
if (LF_buffer == 0) {
  zone_r <- terra::rast(file.path(
    folder_mapzones,
    "LF_zones_0km_rasterized.tif"
  ))
} else if (LF_buffer == 90) {
  zone_r <- terra::rast(file.path(
    folder_mapzones,
    "LF_zones_90km_rasterized.tif"
  ))
} else {
  stop(paste0("Unmatched LF buffer selection: ", LF_buffer))
}

### Loop per dist --------------------------------------------------------------

# list of yearly newly-created DIST code rasters
collector_priorities <- vector("list", length = length(lf_dists))

for (i in seq_along(lf_dists)) {
  this_dist_filepath <- lf_dists[[i]]
  this_dist_year <- get_dist_year(this_dist_filepath)
  this_dist_release <- get_dist_release(this_dist_filepath)

  print(paste0(
    "Starting ",
    i,
    " of ",
    length(lf_dists),
    " at ",
    Sys.time(),
    "."
  ))

  this_dist_csv <- find_dist_csv(this_dist_filepath)
  this_dist_r <- terra::rast(this_dist_filepath)

  ## Dynamic crosswalk to DIST codes

  # D-TIME is by zone group!
  # Plan: Make two full DIST crosswalks, one for SE, one for rest
  #  (only differs in third D-TIME digit)
  # Then mask each and reclassify
  # Then merge back together

  # Time component
  yr_diff <- as.numeric(target_year) - as.numeric(this_dist_year)

  #set up reclassification matrices
  rcl_nse <- set_up_rcl(
    dist_csv = this_dist_csv,
    region = "nonsoutheast",
    yr_diff
  )
  rcl_se <- set_up_rcl(dist_csv = this_dist_csv, region = "southeast", yr_diff)

  ## Non-southeast & southeast, mask & reclassify, & merge

  # ensure values is active category
  terra::activeCat(this_dist_r) <- 0

  # landfire disturbance data needs to match landfire zone data (xmin)
  this_dist_r <- terra::extend(this_dist_r, zone_r)

  # just the non-southeast
  dist_r_nse <- terra::mask(
    this_dist_r,
    zone_r,
    maskvalues = southeast_zones,
    inverse = FALSE
  )
  # specifically dropping any disturbance not in our crosswalks
  codes_r_nse <- terra::classify(dist_r_nse, rcl = rcl_nse, others = NA)

  # just the southeast
  dist_r_se <- terra::mask(
    this_dist_r,
    zone_r,
    maskvalues = southeast_zones,
    inverse = TRUE
  )
  # specifically dropping any disturbance not in our crosswalks
  codes_r_se <- terra::classify(dist_r_se, rcl = rcl_se, others = NA)

  # merge non-southeast and southeast back together
  # can use merge(algo=2) instead of mosaic() since no overlap
  codes_r <- terra::merge(codes_r_nse, codes_r_se, algo = 2)
  names(codes_r) <- "DIST"
  varnames(codes_r) <- "DIST"

  # Save out final save if 1 year OR if per year save requested
  if ((length(lf_dists) == 1) || (save_per_year)) {
    terra::writeRaster(
      codes_r,
      file.path(
        folder_out,
        paste0("DIST_coded_", this_dist_release, "_", this_dist_year, ".tif")
      ),
      gdal = c("COMPRESS=DEFLATE")
    )
  }

  # Multiple years, will need to reclass into priorities for collapsing later
  if (length(lf_dists) > 1) {
    #  reclass each year to DIST priority values
    rcl_rank <- code_to_priority %>%
      dplyr::select(dist_codes, dist_rank) %>%
      as.matrix()
    codes_r_priority <- terra::classify(
      codes_r,
      rcl = rcl_rank,
      others = NA
    )
    # put into collector
    collector_priorities[[i]] <- codes_r_priority
  }
}

### Collapsing multiple years --------------------------------------------------

# create stack
stack_priority <- terra::rast(collector_priorities)
#  take MIN across all rasters (take highest priority)
priority_min <- terra::app(stack_priority, fun = "min", na.rm = TRUE)
#  unclassify (reclassify) back to DIST codes
rcl_unrank <- code_to_priority %>%
  dplyr::select(dist_rank, dist_codes) %>%
  as.matrix()
dist_coded_r <- terra::classify(
  priority_min,
  rcl = rcl_unrank,
  others = NA
)

# Save out
names(dist_coded_r) <- "DIST"
varnames(dist_coded_r) <- "DIST"
terra::writeRaster(
  dist_coded_r,
  file.path(
    folder_out,
    paste0("DIST_coded_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
