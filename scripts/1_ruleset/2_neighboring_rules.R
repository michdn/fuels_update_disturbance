# Take each LF zone rules and adds rules from the 
#  5 closest neighbors to each zone. 
# Encodes the rule criteria. 
#   Uses double-encoding to reduce the number of digits needed.
# DIST: 3
# BPS: 4
# (EF)VH: 2 (of 3)
# (EF)VC: 2 (of 3)
# (EF)VT: 3 (of 4)
# 
# based on old firefactor-fuels src/qa/cmb_table_qa.py
# per original code:
# " the actual values being used in the FM40 crosswalk are the FVH, FVC, FVT
# however, the tables have EVH, EVC, EVT" 

# https://www.landfire.gov/data
#  Shapefiles
# "CONUS Mapzone Shapefile"

#TODO
# The encoding could use a refactor since doing the same logic twice
#  with current zone and then with neighbor zones. 

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  stringr,
  sf,
  nngeo,
  purrr)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

#5 Was used in GEE python since beginning, edit with extreme caution
n_neighbors <- 5

### Settings ----------------------------------------

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

### Double-encoding crosswalks --------------------------

# Encodes/Reclassifies FVH, FVT, FVC with short encoding ID
#  This makes the encoded value small enough to not hit size/digit limits

#Get crosswalks for reclassifying
file_fvh_xwalk <- list.files(
  path = folder_xwalk,
  pattern = paste0("^LF", version_target, "_fvh_xwalk\\.csv$"),
  full.names = TRUE)
file_fvc_xwalk <- list.files(
  path = folder_xwalk,
  pattern = paste0("^LF", version_target, "_fvc_xwalk\\.csv$"),
  full.names = TRUE)
file_fvt_xwalk <- list.files(
  path = folder_xwalk,
  pattern = paste0("^LF", version_target, "_fvt_xwalk\\.csv$"),
  full.names = TRUE)

fvh_xwalk <- read_csv(file_fvh_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvh_ufid) 

fvc_xwalk <- read_csv(file_fvc_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvc_ufid) 

fvt_xwalk <- read_csv(file_fvt_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvt_ufid) 

# Note: BPS -1111 will be recoded to 8888 as well


### Zones & neighbors ------------------------------------

#centroids
zone_c <- st_centroid(zones_sf)

#just need closest ones, without distance, st_nn() easier than st_distance()

# Note: st_nn() returns self as one of the neighbors
#k=number of neighbors PLUS ONE, since we will be dropping self, returns INDEX
zone_k6_raw <- nngeo::st_nn(zone_c, zone_c, k=(n_neighbors+1))

#dropping the first element of each (which is the self)
# in order of zones_sf/zone_c
zone_k5 <- zone_k6_raw %>% 
  purrr::map(function(x) x[2:6])

#st_nn returns INDEX (row number) values, so making an xwalk
zone_c_idx <- zone_c %>% 
  st_drop_geometry() %>% 
  # st_nn() returns index, so using zone_c in SAME ORDER
  mutate(idx = row_number()) %>% 
  dplyr::select(idx, everything()) 


### Loop zones, creating our rules, appending neighbors -------

# loop by index / row number of our zones
# be careful which numbers are indices and which are zone_number! 
# must be by index, because that's order of zone_k5

for (i in 1:nrow(zone_c_idx)){
  
  #the zone 
  this_zone_c <- zone_c_idx %>% 
    filter(idx == i)
  
  this_zone_num <- this_zone_c %>% 
    pull(ZONE_NUM)
  
  #padded to two digits
  this_zone_pad <- this_zone_num %>% 
    stringr::str_pad(2, "left", pad = "0")
  
  #the neighbors, in indices
  this_neighbors <- zone_k5[[i]]
  
  # first, let's prep the encoding for the core zone
  
  # read in this zone cmb & ditch unneeded cols
  this_cmb <- read_csv(
    file.path(folder_cmb,
              paste0("LF", version_target, 
                     "_z", this_zone_pad,
                     "_CMB.csv")),
    show_col_types = FALSE) %>% 
    dplyr::select(-any_of(drop_cols))
  
  #double encoding
  this_cmb <- this_cmb %>% 
    left_join(fvh_xwalk,
              by = join_by("EVHR" == "VALUE")) %>% 
    #filter(is.na(fvh_ufid))
    left_join(fvc_xwalk,
              by = join_by("EVCR" == "VALUE")) %>% 
    #filter(is.na(fvc_ufid))
    left_join(fvt_xwalk,
              by = join_by("EVTR" == "VALUE")) %>% 
    #filter(is.na(fvt_ufid))
    mutate(bps_ufid = if_else(BPSRF==-1111, 8888, BPSRF)) %>% 
    #large values
    mutate(encoded = 
             DIST     * 1e11 + 
             bps_ufid *  1e7 + 
             fvh_ufid *  1e5 + 
             fvc_ufid *  1e3 + 
             fvt_ufid *  1e0) %>% 
    dplyr::select(Zone, encoded, DIST, 
                  BPSRF, bps_ufid, 
                  EVHR, fvh_ufid,
                  EVCR, fvc_ufid,
                  EVTR, fvt_ufid,
                  everything())
  
  # DIST: 3
  # BPS: 4
  # [EF]VH: 2 (of 3 orig)
  # [EF]VC: 2 (of 3 orig)
  # [EF]VT: 3 (of 4 orig)
  # 111*1e0 + 22*1e3 + 33*1e5 + 4444*1e7 + 555*1e11
  
  # second, grab the neighboring zones
  # (j is index of this_neighbors vector)
  for (j in seq_along(this_neighbors)){
    
    this_n_index <- this_neighbors[[j]]
    
    this_n_zone_num <- zone_c_idx %>% 
      filter(idx == this_n_index) %>% 
      pull(ZONE_NUM)
    
    this_n_zone_pad <- this_n_zone_num %>% 
      str_pad(2, pad = "0")
    
    this_n_cmb <- read_csv(
      file.path(folder_cmb,
                paste0("LF", version_target, 
                       "_z", this_n_zone_pad,
                       "_CMB.csv")),
      show_col_types = FALSE) %>% 
      dplyr::select(-any_of(drop_cols)) %>% 
    #double encoding
      left_join(fvh_xwalk,
                by = join_by("EVHR" == "VALUE")) %>% 
      #filter(is.na(fvh_ufid))
      left_join(fvc_xwalk,
                by = join_by("EVCR" == "VALUE")) %>% 
      #filter(is.na(fvc_ufid))
      left_join(fvt_xwalk,
                by = join_by("EVTR" == "VALUE")) %>% 
      #filter(is.na(fvt_ufid))
      mutate(bps_ufid = if_else(BPSRF==-1111, 8888, BPSRF)) %>% 
      #large values
      mutate(encoded = 
               DIST     * 1e11 + 
               bps_ufid *  1e7 + 
               fvh_ufid *  1e5 + 
               fvc_ufid *  1e3 + 
               fvt_ufid *  1e0) %>% 
      dplyr::select(Zone, encoded, DIST, 
                    BPSRF, bps_ufid, 
                    EVHR, fvh_ufid,
                    EVCR, fvc_ufid,
                    EVTR, fvt_ufid,
                    everything())
    
    #append neighbor 
    this_cmb <- bind_rows(this_cmb, 
                          this_n_cmb)
    
    #remove duplicates (favoring earlier entries)
    this_cmb <- this_cmb %>% 
      #distinct on encoded, keep all variables,
      #  it keeps the first row 
      distinct(encoded, .keep_all=TRUE)
    
    
    # NewCanopy LF2024 has new value 9999 in some zones?
    #  In older versions, I believe these would have been 0
    this_cmb <- this_cmb %>% 
      mutate(NewCanopy = if_else(NewCanopy==9999, 0, NewCanopy))
    
    
  }# end j neighbors
  
  #save out zone with neighbor rules
  write_csv(this_cmb,
            file.path(folder_out,
                      paste0("LF", version_target, 
                             "_z", this_zone_pad,
                             "_n_CMB.csv")))
  
  print(paste0("Finished zone ", this_zone_num, ". ", 
              i, " of ", nrow(zone_c_idx), "."))

} # end i zones
