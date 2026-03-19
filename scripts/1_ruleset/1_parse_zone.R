# LANDFIRE Total Fuel Change Tool Master_CMB table
#  split out by zone

# Previous to this script the Master_CMB table from the
#  MS Access LFTFCT ruleset database must have been extracted:
# 1. Open .mdb in MS Access
# 2. Right-click each table name (e.g. Master_CMB) and Export
# 3. Pick text file, delimited, comma, and INCLUDE headers on first row
# 4. It will save as a .txt. Manually change to .csv afterwards.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  stringr
)

source(file.path("scripts", "common_vars_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

### Set up ---------------------------------------------------------------------

prefix <- paste0("LF", version_target, "_")

# Set up an outfolder with LF year name
folder_out <- file.path(
  folder_lfrules_base,
  paste0("rules_extracted_LF", version_target)
)
dir.create(folder_out)

# Find target CMB export based on version_target
folder_raw_db <- list.files(
  path = folder_lfrules_base,
  pattern = paste0("^Fuel_Rulesets_Database_", version_target, "$"),
  include.dirs = TRUE
)

file_raw_cmb <- list.files(
  path = file.path(folder_lfrules_base, folder_raw_db),
  pattern = "^Master_CMB\\.csv$",
  full.names = TRUE
)

cmb <- readr::read_csv(file_raw_cmb)

### Parse out each zone --------------------------------------------------------

zonelist <- cmb %>% pull("Zone") %>% unique() %>% sort()
#Note: could have used `zones` from common_vars_functions.R but
# Master_CMB has zones from everywhere not just CONUS

for (i in seq_along(zonelist)) {
  this_zone <- zonelist[i]

  print(paste0('Starting i = ', i, " & zone = ", this_zone))

  this_cmb <- cmb %>%
    filter(Zone == this_zone)

  print(paste0('This zone has ', nrow(this_cmb), ' records'))

  this_zone_pad <- str_pad(this_zone, 2, pad = "0")

  readr::write_csv(
    this_cmb,
    file = file.path(
      folder_out,
      paste0(prefix, "z", this_zone_pad, "_CMB.csv")
    )
  )
}
