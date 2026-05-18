# Filtering fire perimeters
#  (from burn-severity-gee pull nifc fires)

# From yearly 2025 to only 2025 post end of fiscal year
#   (LF products run to fiscal year)
# So Oct-December fires

# Will upload these Oct-Dec perimeters to GEE
#  to use burn-severity-gee to calculate
#  estimated burn severity

# Then we will use another script(s) here
#  to create DIST codes and combine with LF disturbances.

### Packages & Function --------------------------------------------------------

if (!require("pacman")) {
  install.packages("pacman")
}
pacman::p_load(
  tidyverse,
  sf
)

source(file.path("scripts", "common_vars_functions.R"))

### User settings --------------------------------------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

### Data in --------------------------------------------------------------------

perims <- sf::read_sf(file.path(
  folder_out_base,
  version_proj,
  "disturbance",
  "fire_perims",
  "nifc_fires_2025_gte100ac_20260313.shp"
))

### Filter and save ------------------------------------------------------------

#post fiscal year
# MAKE SURE to keep Discovery field as character!
pfy25 <- perims %>%
  dplyr::mutate(Disc_date = as.Date(Discovery)) %>%
  dplyr::filter(Disc_date >= as.Date("2025-10-01")) %>%
  dplyr::arrange(Disc_date) %>%
  dplyr::select(-Disc_date)

sf::write_sf(
  pfy25,
  file.path(
    folder_out_base,
    version_proj,
    "disturbance",
    "fire_perims",
    "nifc_fires_2025postFY_gte100ac_20250313.shp"
  )
)
