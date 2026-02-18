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
# version_dist is the LF year
version_dist <- "2025"
# dist_vers is the version of disturbance data c("LDist", "PDist", "Dist")
#  "Limited": "LDist", 
#  "Preliminary": "PDist", 
#   "Annual": "Dist"
dist_vers <- "LDist"

### D_TYPE & D_SEVERITY xwalks -------------------------

# D_TYPE
# 1: Fire, 2: Mechanical Add, 3: Mechanical Remove, 4: Windthrow, 
# 5: Insects-Disease, 6: Mechanical Unknown, 7: Mastication
dtype_xwalk <- tribble(
  ~type_dist, ~type_code,
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
  ~level_sev, ~sev_code, 
  #"Unburned/Low", "?", #dropping 
  "Low", 1, 
  "Moderate", 2, 
  "High", 3)

#dropping "No Severity", "Increased Green"

#D_TIME
# Is most likely always 1 if doing recent updates
dtime_xwalk <- tibble(
  years_time = 1:10, 
  time_code = c(1, rep(2, 4), rep(3, 5)))


### Data in ------------------------------------------

#folder and file name depends on the LF year and dist version


#csv crosswalk



# Get NoData (-9999), NA (0), and Water mask value (sometimes 2001)


# Find target based on version_target

file_dist_csv <- list.files(
  path = folder_landfire,
  pattern = paste0("^LF", version_target, 
                   #last two digits of year
                   "_Dist", str_sub(version_target, start=-2, end=-1),
                   "_CONUS\\.tif$"),
  full.names = TRUE)

bps <- rast(file_bps)




### Dynamic crosswalk to DIST codes ------------------

# Descriptions and other column values haven't changed. 
#  If they do, these mini xwalks will need to be updated/dynamic.



### Reclassify LF disturbance to DIST codes ----------



# Save out





