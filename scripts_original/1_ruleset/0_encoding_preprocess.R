# Dynamic encoding rules

# Looking at official CSV metadata of LF data for FV* data
# Uncertain if changes over time, so doing dynamic double-encoding.

## NEW encoding plan ---

# 14 Digits needed with double-encoding of FV*:

#DIST: 3
#BPS: 4
#(EF)VH: 2 (of 3)
#(EF)VC: 2 (of 3)
#(EF)VT: 3 (of 4)

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

### Dynamic encoding crosswalks ------------------------------------------------

# FVH, FVC, FVT dynamic recoding

# Read version csv data
file_fvh_csv <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVH\\.csv$"),
  recursive = TRUE,
  full.names = TRUE
)
file_fvc_csv <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVC\\.csv$"),
  recursive = TRUE,
  full.names = TRUE
)
file_fvt_csv <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVT\\.csv$"),
  recursive = TRUE,
  full.names = TRUE
)

fvh_csv <- readr::read_csv(file_fvh_csv, show_col_types = FALSE)
fvc_csv <- readr::read_csv(file_fvc_csv, show_col_types = FALSE)
fvt_csv <- readr::read_csv(file_fvt_csv, show_col_types = FALSE)

# Create crosswalks
fvh_xwalk <- dplyr::bind_rows(
  fvh_csv %>%
    dplyr::filter(VALUE == -9999) %>%
    #edit -9999 to have id of NA (will reclass to NA in later step)
    dplyr::mutate(fvh_ufid = NA) %>%
    dplyr::select(fvh_ufid, everything()),
  fvh_csv %>%
    dplyr::filter(!VALUE == -9999) %>%
    dplyr::mutate(fvh_ufid = dplyr::row_number()) %>%
    dplyr::select(fvh_ufid, everything())
)

fvc_xwalk <- dplyr::bind_rows(
  fvc_csv %>%
    filter(VALUE == -9999) %>%
    #edit -9999 to have id of NA (will reclass to NA in later step)
    mutate(fvc_ufid = NA) %>%
    dplyr::select(fvc_ufid, tidyselect::everything()),
  fvc_csv %>%
    filter(!VALUE == -9999) %>%
    mutate(fvc_ufid = dplyr::row_number()) %>%
    dplyr::select(fvc_ufid, tidyselect::everything())
)

fvt_xwalk <- dplyr::bind_rows(
  fvt_csv %>%
    filter(VALUE == -9999) %>%
    #edit -9999 to have id of NA (will reclass to NA in later step)
    mutate(fvt_ufid = NA) %>%
    dplyr::select(fvt_ufid, tidyselect::everything()),
  fvt_csv %>%
    filter(!VALUE == -9999) %>%
    mutate(fvt_ufid = dplyr::row_number()) %>%
    dplyr::select(fvt_ufid, tidyselect::everything())
)

# Save out
readr::write_csv(
  fvh_xwalk,
  file.path(
    folder_xwalk,
    paste0("LF", version_target, "_fvh_xwalk.csv")
  )
)
readr::write_csv(
  fvc_xwalk,
  file.path(
    folder_xwalk,
    paste0("LF", version_target, "_fvc_xwalk.csv")
  )
)
readr::write_csv(
  fvt_xwalk,
  file.path(
    folder_xwalk,
    paste0("LF", version_target, "_fvt_xwalk.csv")
  )
)
