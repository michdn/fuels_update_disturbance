# FM40 update
# Loops by zone
# Writes out each zone

# total ~3.5 hours: 
# 1-1.5 hours preprocessing steps, 1.5-2 hrs for all zone loop

# See next script to mosaic

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

#save out encoded pixels that do not match rules, per zone
save_unmatched <- FALSE
#save a copy of only-updated disturbed pixels (does not affect final output)
save_updated <- FALSE


### Settings -----------------------------------------

#preventing scientific notation
options(scipen=999)

#To avoid issues with large numbers, tell terra to use
#  64 bit numbers, otherwise will see weird rounding on the 
#  very large encoded numbers. 
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terraOptions(datatype="FLT8S") #FLT8S 

### Data in --------------------------------------------

# Find target based on version_target
#  in pre-processed LANDFIRE data

file_bps <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_bps, "_bps_coded\\.tif$"),
  full.names = TRUE)

bps <- rast(file_bps)

# Veg
# per GEE code: 
# "the actual values being used in the FM40 crosswalk are the FVH, FVC, FVT
# however, the tables have EVH, EVC, EVT...
# so variables are named as in the tables but note they are actually the F* layers"

file_evc <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvc_coded\\.tif$"),
  full.names = TRUE)

file_evh <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvh_coded\\.tif$"),
  full.names = TRUE)

file_evt <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fvt_coded\\.tif$"),
  full.names = TRUE)

evc <- rast(file_evc)
evh <- rast(file_evh)
evt <- rast(file_evt)


# Existing FBFM40.
file_fbfm40 <- list.files(
  path = folder_lfproc,
  pattern = paste0("^LF", version_target, "_fbfm\\.tif$"),
  full.names = TRUE)

fbfm <- rast(file_fbfm40)

# Disturbance

# TODO TEST DATA
# probably set for real in project parameters? 
# dist_raw <- rast("data/test_data/dist_outputs_dist_all_202410v4_tiles_dist_all_202410v4.tif")
# #it's test data, force for now
# dist_align <- project(dist_raw, evh, method="near")
# ext(evh)
# ext(dist_align)
# dist_align
# writeRaster(dist_align, file.path("data", "test_data", "dist202410v4.tif"))
#dist <- dist_align
dist <- rast(file.path("data", "test_data", "dist202410v4.tif"))


### Encode ---------------------------------------------------

# TODO
# May move into own preprocess script, since used in 
#  multiple fuel layer updates??


#42-70 min for stack & sum (depending on what else is running, chrome/RAM, etc)
# (faster than all in one calc by over 20 min)
r_stack <- c(dist * 1e11,
             bps  *  1e7,
             evh  *  1e5,
             evc  *  1e3,
             evt  *  1e0)
r_coded <- terra::app(r_stack, fun = "sum")

# #save out for dev/QA use
terra::writeRaster(
  r_coded,
  file.path("data", "test_data", "r_coded_flt8s.tif"),
  gdal=c("COMPRESS=DEFLATE"),
  #set for largest possible values given size of encoded values
  datatype = "FLT8S",
  overwrite = TRUE)
r_coded
r_coded <- rast(file.path("data", "test_data", "r_coded_flt8s.tif"))
# r_coded
# 13227233762386 max


## Preprocess Encoded where Disturbance 

# Rather than masking each zone inside the loop. 
#where no disturbance, NA, otherwise encoded value
#mask (17 min) is much faster than ifel (50 min)
coded_dist <- mask(r_coded, dist, maskvalues=0, inverse=FALSE)

# #save out for dev/QA use
terra::writeRaster(
  coded_dist,
  file.path("data", "test_data", "coded_dist_flt8s.tif"),
  gdal=c("COMPRESS=DEFLATE"),
  #set for largest possible values given size of encoded values
  datatype = "FLT8S",
  overwrite = TRUE)
coded_dist
coded_dist <- rast(file.path("data", "test_data", "coded_dist_flt8s.tif"))


### Update by zone -------------------------------------------------

(start_time <- Sys.time())
for (i in seq_along(zones)){
  
  this_zone_num <- zones[[i]]
  this_zone_pad <- str_pad(this_zone_num, 2, "left", 0)
  
  print(paste0("Starting zone ", this_zone_pad, ". Zone ", 
               i, " of ", length(zones), 
               " at ", Sys.time()))
  
  this_zone_sf <- zones_sf %>% 
    filter(ZONE_NUM == this_zone_num)
  
  #get rules for this zone (plus neighbors)
  this_rules <- read_csv(
    file.path(folder_lfrules_base,
              paste0("rules_wneighbors_LF", version_target),
              paste0("LF", version_target, "_z", 
                     this_zone_pad, "_CMB.csv")),
    show_col_types = FALSE) #%>% 

  #reclassification rules matrix
  this_rcl <- this_rules %>% 
    dplyr::select(encoded, NewFBFM40) %>% 
    as.matrix()
  
  #read in this zone raster (for masking)
  this_z_r <- rast(
    file.path("data", "lf_mapzones", "per_zone_rasters",
              paste0("z", this_zone_pad, ".tif")))
  
  #crop the disturbed-only critera raster to this zone extent (not mask here)
  # <1 min
  this_coded_dist <- crop(coded_dist, this_z_r)
  
  #mask the criteria (disturbed-only) raster to this zone
  # a few secs
  this_enc_d <- mask(this_coded_dist, this_z_r, 
                     maskvalues = this_zone_num, inverse = TRUE)
  
  #reclassify just the zone (just the disturbed pixels)
  #3-4 min
  this_updt <- classify(this_enc_d, rcl = this_rcl, others = -1)
  
  #some do not have rule combinations that exist
  # (zone 5 test case - none existed in all CONUS rules)
  # save out here if wanted
  if (save_unmatched) {
    folder_qa <- file.path("data", "test_data", "unmatched_rules")
    dir.create(folder_qa, showWarnings = FALSE, recursive=TRUE)
    
    this_err <- mask(this_enc_d, this_updt, maskvalues = -1, inverse = TRUE)
    
    writeRaster(
      this_err, 
      file.path(folder_qa,
                paste0("unmatched_encoding_z", this_zone_pad, ".tif")),
      gdal=c("COMPRESS=DEFLATE"),
      datatype = "FLT8S",
      overwrite = TRUE)
  } # end if save_unmatched
  
  #remove unmatched markers
  this_updt <- mask(this_updt, this_updt, maskvalues = -1)
  
  #raster names
  names(this_updt) <- "new_fbfm40"
  varnames(this_updt) <- "new_fbfm40"
  
  if (save_updated) {
    folder_ud <- file.path(folder_out_base, version_proj, 
                           "FBFM40_update_zone_onlydist")
    dir.create(folder_ud, showWarnings = FALSE, recursive=TRUE)
    
    writeRaster(
      this_updt,
      file.path(folder_out, 
                paste0("fbfm40_z", this_zone_pad, "_updatedonly.tif")),
      gdal=c("COMPRESS=DEFLATE"),
      #python .int16() #changed from .uint16() per request of Chris L. for 2024 run
      # R terra equivalent is INT2U. 
      datatype = "INT2U")
  } # end if save_unmatched
  
  
  ## Zone update and cover with baseline FBFM40 here, CONUS mosaic afterwards
  ## Add in baseline FBFM40
  #crop baseline fbfm to zone extent
  this_fbfm_crop <- crop(fbfm, this_z_r)
  #and mask it to just the zone
  # this prevents non-zone pixels from ending up in the cover()
  # and means that the zone tifs will not overlap
  this_fbfm <- mask(this_fbfm_crop, this_z_r, 
                     maskvalues = this_zone_num, inverse = TRUE)
  #fill in with baseline values anywhere inside the zone 
  #  where there is no updated values (ie. not disturbed)
  this_updt_fbfm <- cover(this_updt, this_fbfm, values = NA)

  # Save out
  folder_out <- file.path(folder_out_base, version_proj, "FBFM40_update_zone")
  dir.create(folder_out, showWarnings = FALSE, recursive = TRUE)
  
  writeRaster(
    this_updt_fbfm,
    file.path(folder_out, 
              paste0("fbfm40_z", this_zone_pad, ".tif")),
    gdal=c("COMPRESS=DEFLATE"),
    #python .int16() #changed from .uint16() per request of Chris L. for 2024 run
    # R terra equivalent is INT2U. 
    datatype = "INT2U")
  
  # TODO
  #QA flags
  # 1: disturbed and UPDATED value
  # 2: disturbed but SAME value as before
  # merge with -1 unmatched rules above??
  
} # end per zone (i)
(end_time <- Sys.time())
(end_time - start_time)
