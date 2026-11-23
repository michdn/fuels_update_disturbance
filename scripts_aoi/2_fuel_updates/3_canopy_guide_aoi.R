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

lapply(
  X = zones_lim,
  FUN = build_guide_by_zone,
  version_target = version_target,
  coded_dist = coded_dist,
  dist = dist,
  folder_cmb = folder_cmb,
  folder_zones = folder_zones,
  folder_out = folder_out,
  save_qa = TRUE,
  folder_qa = file.path(folder_out_base, version_proj, "qa")
)

### Merge ----------------------------------------------------------------------

# Read in all created above

files_cg <- list.files(
  path = folder_out,
  pattern = "^canopyguide_z.*\\.tif$",
  full.names = TRUE
)

if (!length(files_cg) == length(zones_lim)) {
  stop(paste0(
    "Incorrect number of canopy guide per zone rasters. Found ",
    length(files_cg),
    ", but",
    length(zones_lim),
    "were expected."
  ))
}
#put all into a raster collection (this does not mind different extents)
sprc_cg <- do.call(terra::sprc, list(files_cg))

#Do not use algo 1 as it resamples. Use algo 3 if a vrt is needed.
# As the zones were rasterized, and that raster was used to
#  mask the per zone steps, we know there is no issues with
#  e.g. sf crop and ending up with missing or overlapping pixels.
# Can use the simpler and faster merge (but NOT algo 1 which resamples).
# Will have to fix outer extents (it will shave off edges if no data).
cg_merged <- terra::merge(sprc_cg, algo = 2)

# Fixing extents (can trim off edges in CONUS test cases, so keeping here just in case)
# using aoi clipped dist
cg_aoi <- terra::extend(cg_merged, dist)
varnames(cg_aoi) <- "canopy_guide"

#Save
terra::writeRaster(
  cg_aoi,
  file.path(
    folder_out_base,
    version_proj,
    "processing",
    paste0("canopy_guide_", version_proj, ".tif")
  ),
  gdal = c("COMPRESS=DEFLATE")
)
