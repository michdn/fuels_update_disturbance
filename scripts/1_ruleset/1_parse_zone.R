# LANDFIRE Total Fuel Change Tool Master_CMB table
#  split out by zone


### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

prefix <- paste0("LF", version_target, "_") 


### Set up -------------------------------------------

# Set up an outfolder with LF year name
folder_out <- file.path(folder_lfrules_base,
                        paste0("rules_extracted_LF",
                               version_target))
dir.create(folder_out)



# Find target CMB export based on version_target
folder_raw_db <- list.files(
  path = folder_lfrules_base,
  pattern = paste0("^Fuel_Rulesets_Database_",
                   version_target, "$"),
  include.dirs = TRUE)

file_raw_cmb <- list.files(
  path = file.path(folder_lfrules_base,
                   folder_raw_db),
  pattern = "^Master_CMB\\.csv$",
  full.names = TRUE)


cmb <- read_csv(file_raw_cmb)

cmb


### Parse out each zone --------------------------------

zonelist <- cmb %>% pull("Zone") %>% unique() %>% sort()

for (i in seq_along(zonelist)){

  this_zone <- zonelist[i]

  print(paste0('Starting i = ', i, " & zone = ", this_zone))

  this_cmb <- cmb %>%
    filter(Zone == this_zone)

  print(paste0('This zone has ', nrow(this_cmb), ' records'))

  this_zone_pad <- str_pad(this_zone, 2, pad = "0")
  write_csv(this_cmb,
            file = file.path(folder_out,
                             paste0(prefix, "z", this_zone_pad, "_CMB.csv")))

  rm(this_cmb)

}


