# LF annual disturbance
# Creating DIST codes from LF dist data

# Note: Codes changed in LF2024, so creating DYNAMIC crosswalks
# Note2: Naming scheme changed in some places (per year Annual Disturbance)
#        but not in others (All Years Annual Disturbance). 
#        Code below assumes the per year download scheme. 


#TODO
# different naming between Annual and Limited
# different values between Annual and Limited
#      common ones remain, but Limited has set that Annual does not


#TODO Currently, this only accepted a single disturbance

# If this creates the final disturbance, update the file pointer in 
#  the project parameter R script
# This is a MANUAL change. 


### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  stringr,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -------------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

## LANDFIRE disturbance
# dist_year is the LF year
dist_year <- "2025"
# dist_vers is the version of disturbance data c("LDist", "PDist", "Dist")
#  "Limited": "LDist", 
#  "Preliminary": "PDist", 
#  "Annual": "Dist"
dist_vers <- "LDist"

### D_TYPE & D_SEVERITY xwalks -------------------------

# D_TYPE
# 1: Fire, 2: Mechanical Add, 3: Mechanical Remove, 4: Windthrow, 
# 5: Insects-Disease, 6: Mechanical Unknown, 7: Mastication
dtype_xwalk <- tribble(
  ~DIST_TYPE, ~D_TYPE,
  "Fire", 1, 
  "Wildfire", 1, 
  "Wildland Fire Use", 1, 
  "Prescribed Fire", 1, 
  "Mechanical Add", 2,
  "Thinning", 2, 
  "Mechanical Remove", 3, 
  "Clearcut", 3, 
  "Development", 3, 
  "Harvest", 3, 
  "Weather", 4, 
  "Insects/Disease", 5,
  "Insects", 5, 
  "Disease", 5, 
  "Mechanical Unknown", 6, 
  "Insecticide", 6, 
  "Chemical", 6, 
  "Herbicide", 6, 
  "Biological", 6, 
  "Unknown", 6, 
  "Mastication", 7)

#D_SEVERITY, 2nd digit
# 1: Low, 2: Moderate, 3: High
dsev_xwalk <- tribble(
  ~SEVERITY, ~D_SEVERITY, 
  #"Unburned/Low", "?", #dropping 
  "Low", 1, 
  "Moderate", 2, 
  "High", 3)

#dropping "No Severity", "Increased Green"

#D_TIME
# 1 if doing recent updates. Not fully built to handle multiple years.
dtime_xwalk <- tibble(
  diff_years = 1:10, 
  D_TIME = c(1, rep(2, 4), rep(3, 5)))

### Data in ------------------------------------------

#folder and file name depends on the LF year and dist version
# eg limited like LF2025_LDist25_CONUS.tif
# prelim like LF2024_PDist24_CONUS.tif
# final annual like LF2024_Dist24_CONUS.tif
# HOWEVER, if downloading from compilations, the file names are different!
#  this script does not handle these (eg LC24_Dist_250.tif), ONLY yearly ones. 

# Find targets based on dist_year and dist_vers
fname_base <- paste0("LF", dist_year,  
                     "_", dist_vers, 
                     #last two digits of year
                     str_sub(dist_year, start=-2, end=-1))

#csv data from landfire
file_dist_csv <- list.files(
  path = folder_lfdata_base,
  pattern = paste0(fname_base, "\\.csv$"), 
  full.names = TRUE,
  recursive = TRUE)
dist_csv <- read_csv(file_dist_csv)

#disturbance raster
file_dist_rast <- list.files(
  path = folder_lfdata_base,
  pattern = paste0(fname_base, "_CONUS\\.tif$"), 
  full.names = TRUE,
  recursive = TRUE)
dist_r <- rast(file_dist_rast)


### Dynamic crosswalk to DIST codes ------------------

#time component (single year only)
yr_diff <- as.numeric(dist_year) - as.numeric(version_target)

# disturbance type and severity
# Descriptions and other column values haven't changed from LF dist versions. 
#  If they do, the mini xwalks above will need to be updated/dynamic.
dist_dist <- dist_csv %>% 
  dplyr::select(VALUE, DIST_TYPE, SEVERITY) %>% 
  #add year difference from LF version and disturbance version, calc above
  mutate(diff_years = .env$yr_diff) %>% 
  #join all xwalks, dropping those unmatched (don't want NAs)
  inner_join(dtype_xwalk, by = join_by(DIST_TYPE)) %>% 
  inner_join(dsev_xwalk, by = join_by(SEVERITY)) %>% 
  inner_join(dtime_xwalk, by = join_by(diff_years)) %>% 
  # make DIST code
  mutate(DIST = paste0(D_TYPE, D_SEVERITY, D_TIME) %>% as.numeric())

# Get NoData (-9999), NA (0), and Water mask value (sometimes 2001)
dist_spec <- dist_csv %>% 
  filter(DIST_TYPE %in% c("Fill-NoData")) %>% 
  mutate(DIST = NA_real_) %>% 
  rows_append(dist_csv %>% 
                filter(DIST_TYPE %in% c(NA, "Water")) %>% 
                mutate(DIST = 0)) %>% 
  dplyr::select(VALUE, DIST_TYPE, SEVERITY, DIST)

# Full xwalk for reclassifying (and checking)
dist_codes <- bind_rows(dist_spec, dist_dist)

# TODO
# NOTE: This will silently drop things that aren't in mini crosswalks above
#  Probably want to build out something more robust or write out unmatched to 
#   review. Some are intentionally dropped (low/unburned, etc.)

### Reclassify LF disturbance to DIST codes ----------

dist_rcl <- dist_codes %>% 
  dplyr::select(VALUE, DIST) %>% 
  as.matrix()

activeCat(dist_r) <- 0
dist_r
dist_r_coded <- classify(dist_r, rcl = dist_rcl, 
                         #NA dropping everything not in our xwalk! Intentional.
                         others = NA)
names(dist_r_coded) <- "DIST"
varnames(dist_r_coded) <- "DIST"
dist_r_coded

# #testing with classify others = -1
# dist_r_coded <- classify(dist_r, rcl = dist_rcl, others = -1)
# fail_r <- mask(dist_r, dist_r_coded, maskvalue = -1, inverse = TRUE)
# fail_r
# fail_freq <- freq(fail_r)
# fail_freq %>% as_tibble() %>% arrange(desc(count))
# # all unburned/low or increased green, all good. 
# writeRaster(
#   fail_r, 
#   file.path("data", "test_data", "lf_dist_qa", "fail_r.tif"),
#   gdal=c("COMPRESS=DEFLATE"),
#   overwrite = TRUE)
# writeRaster(
#   dist_r_coded, 
#   file.path("data", "test_data", "lf_dist_qa", "dist_r_coded.tif"),
#   gdal=c("COMPRESS=DEFLATE"),
#   overwrite = TRUE)


# Save out

folder_out <- file.path(folder_out_base, version_proj, "disturbance")
dir.create(folder_out, showWarnings = FALSE, recursive=TRUE)

writeRaster(
  dist_r_coded,
  file.path(folder_out, 
            paste0(dist_vers, "_", dist_year, "_dist_coded.tif")),
  gdal=c("COMPRESS=DEFLATE"))




