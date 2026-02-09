# Take each LF zone rules and adds rules from the 
#  5 closest neighbors to each zone. 

# firefactor-fuels src/qa/cmb_table_qa.py

# https://www.landfire.gov/data
#  Shapefiles
# "CONUS Mapzone Shapefile"

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  sf,
  nngeo,
  purrr)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

#preventing scientific notation
options(scipen=999)

#column we don't need at all and can immediately drop
drop_cols = c(
  "VALUE",
  "COUNT",
  "WILDCARD",
  #LF2024 had title case
  "Wildcard",
  "SClass",
  "FBFM13",
  "FBFM40",
  "CanFM",
  "FCCS",
  "FLM",
  "CCover",
  "CHeight",
  "CBH13mx10",
  "CBH40mx10",
  "CBD13x100",
  "CBD40x100",
  "Canopy",
  "NewCanFM",
  "NewFCCS",
  "NewFLM",
  "NewCBH13mx10",
  "NewCBD13x100",
  "NewSClass")


### Set up -------------------------------------------

# Set up an outfolder with LF year name
folder_out <- file.path(folder_lfrules_base,
                        paste0("rules_wneighbors_LF",
                               version_target))
dir.create(folder_out)

#Rules data folder (note, name of csvs is by zone number)
folder_cmb <- file.path(folder_lfrules_base,
                        paste0("rules_extracted_LF",
                               version_target))

### Zones & neighbors ------------------------------------

folder_zones <- file.path("data", "lf_mapzones", 
                          "LF_CONUS_mz90k_0k_shps")
# 0k without buffer 
# (probably wouldn't matter too much, but centroids would be different)
# in 5070
zone_sf <- read_sf(file.path(folder_zones, "conus_mz_0k.shp"))

#centroids
zone_c <- st_centroid(zone_sf)

## sf way ---
# #distances of centroids
# # (if of the polygon, 0 for any touching, may have more than 5, 
# #  so must be based on centroid)
# zone_d <- st_distance(zone_c)

## nngeo way ---

#just need closest ones, without distance, st_nn() easier

#k=6 since we will be dropping self
zone_k6_raw <- nngeo::st_nn(zone_c, zone_c, k=6)

#dropping the first element of each (which is the self)
zone_k5 <- zone_k6_raw %>% 
  purrr::map(function(x) x[2:6])

#st_nn returns INDEX (row number) values, so making an xwalk
zone_c_idx <- zone_c %>% 
  st_drop_geometry() %>% 
  mutate(idx = row_number()) %>% 
  dplyr::select(idx, everything())


### Loop zones, creating our rules, appending neighbors -------

# loop by index / row number of our zones
# be careful which numbers are indices and which are zone_number! 

for (i in 1:length(zone_c_idx)){
  
  #the zone 
  this_zone_c <- zone_c_idx %>% 
    filter(idx == i)
  this_zone_num <- this_zone_c %>% 
    pull(ZONE_NUM)
  
  #the neighbors, in indices
  this_neighbors <- zone_k5[[i]]
  
  # first, let's prep the encoding for the core zone
  
  # read in this zone cmb & ditch unneeded cols
  this_cmb <- read_csv(
    file.path(folder_cmb,
              paste0("LF", version_target, 
                     "_z", this_zone_num,
                     "_CMB.csv"))) %>% 
    dplyr::select(-any_of(drop_cols)) %>% 
    #very large values
    mutate(encoded = 
             DIST *  1e13 + 
             BPSRF * 1e10 + 
             EVHR *   1e7 + 
             EVCR *   1e4 + 
             EVTR *   1e0)
  
  # second, grab the neighboring zones
  # (j is index of this_neighbors vector)
  for (j in seq_along(this_neighbors)){
    
    this_n_index <- this_neighbors[[j]]
    
    this_n_zone_num <- zone_c_idx %>% 
      filter(idx == this_n_index) %>% 
      pull(ZONE_NUM)
    
    this_n_cmb <- read_csv(
      file.path(folder_cmb,
                paste0("LF", version_target, 
                       "_z", this_n_zone_num,
                       "_CMB.csv"))) %>% 
      dplyr::select(-any_of(drop_cols)) %>% 
      #very large values
      mutate(encoded = 
               DIST *  1e13 + 
               BPSRF * 1e10 + 
               EVHR *   1e7 + 
               EVCR *   1e4 + 
               EVTR *   1e0)    
    
    #append neighbor 
    this_cmb <- bind_rows(this_cmb, 
                          this_n_cmb)
    
    #remove duplicates (favoring earlier entries)
    this_cmb <- this_cmb %>% 
      #distinct on encoded, keep all variables,
      #  it keeps the first row 
      distinct(encoded, .keep_all=TRUE)
    
  }# end j neighbors
  
  #save out zone with neighbor rules
  write_csv(this_cmb,
            file.path(folder_out,
                      #same name, but different folder! 
                      paste0("LF", version_target, 
                             "_z", this_zone_num,
                             "_CMB.csv")))
  
  #clean up, paranoid with large values
  rm(this_cmb, this_n_cmb)
  gc()
  
} # end i zones