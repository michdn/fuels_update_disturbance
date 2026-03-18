# Creating canopy guide (criteria encoding)
#  for use later in updating CC, CH, CBH, CBD

# Encodes values from DIST, BPS, FVH (coded), FVC (coded), and FVT (coded)
#  into our LFTCT rule encoded value 
#  (See scripts in 1_ruleset for generating the encoded values in the crosswalk,
#    here we are creating the encoded values in a raster.)
#  (See 0_landfire_preprocess.R for encoding of the FVH, FVC, FVT rasters)

# Run time: ~ 6 hours

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", "2026_WRME_LF2024_updt2025.R"))

#save out encoded pixels that do not match rules, per zone
# QA setting, or change directory in writeRaster call
save_unmatched <- FALSE

### Settings ------------------------------------------

#preventing scientific notation
options(scipen=999)

#To avoid issues with large numbers, tell terra to use
#  64 bit numbers, otherwise will see weird rounding on the 
#  very large encoded numbers. 
# ABSOLUTELY CRITICAL SETTING, DO NOT CHANGE
terraOptions(datatype="FLT8S") #FLT8S 

# appropriate zone rasters, parameter set in parameter script file
if (LF_buffer == 0){
  folder_zones <- file.path(folder_mapzones,
                            "per_zone_rasters_0km")
} else if (LF_buffer == 90){
  folder_zones <- file.path(folder_mapzones,
                            "per_zone_rasters_90km")
} else {
  stop(paste0("Unmatched LF buffer selection: ", LF_buffer))
}

# Out-folder. Guide depends on DIST, so project-level, so put in proj folder
folder_out <- file.path(folder_out_base, 
                        version_proj, 
                        "processing", 
                        "canopy_guide")
dir.create(folder_out, showWarnings = FALSE, recursive=TRUE)

### Data in ------------------------------------------

# LFTFC rules by zone (with neighbors)
#location of LFTFCT ruleset, encoded, with neighboring zones
folder_cmb <- file.path(folder_lfrules_base,
                        paste0("rules_wneighbors_LF",
                               version_target))

# Encoded DIST & criteria raster
coded_dist <- rast(file.path(folder_out_base,
                             version_proj,
                             "processing", 
                             "encoded_rule_criteria_distonly.tif"))

# Disturbance
# Set in the project parameters
dist <- rast(dist_file)


### Guide by zone -------------------------------------------------


(start_time <- Sys.time())
for (i in seq_along(zones)){
  
  this_zone_num <- zones[[i]]
  this_zone_pad <- str_pad(this_zone_num, 2, "left", 0)
  
  print(paste0("Starting zone ", this_zone_pad, ". Zone ", 
               i, " of ", length(zones), 
               " at ", Sys.time()))
  
  #get rules for this zone (plus neighbors)
  this_rules <- read_csv(
    file.path(folder_cmb,
              paste0("LF", version_target, "_z", 
                     this_zone_pad, "_n_CMB.csv")),
    show_col_types = FALSE) #%>% 
  
  #reclassification rules matrix
  this_rcl <- this_rules %>% 
    dplyr::select(encoded, NewCanopy) %>% 
    as.matrix()
  
  #read in this zone raster (for masking)
  this_z_r <- rast(
    file.path(folder_zones,
              paste0("z", this_zone_pad, ".tif")))
  
  #crop the disturbed-only criteria raster to this zone extent (not mask here)
  this_coded_dist <- crop(coded_dist, this_z_r)
  
  #mask the criteria (disturbed-only) raster to this zone
  this_enc_d <- mask(this_coded_dist, this_z_r, 
                     maskvalues = this_zone_num, inverse = TRUE)
  
  #reclassify just the zone (just the disturbed pixels)
  this_canopy <- classify(this_enc_d, rcl = this_rcl, others = -1)
  
  #some do not have rule combinations that exist
  # (FBFM40 zone 5 test case - none existed in all CONUS rules)
  # save out here if wanted
  if (save_unmatched) {
    folder_qa <- file.path("data", "test_data", "unmatched_canopy_rules")
    dir.create(folder_qa, showWarnings = FALSE, recursive=TRUE)
    
    this_err <- mask(this_enc_d, this_canopy, maskvalues = -1, inverse = TRUE)
    this_err_freq <- freq(this_err) %>% as_tibble()
    
    # writeRaster(
    #   this_err, 
    #   file.path(folder_qa,
    #             paste0("unmatched_encoding_z", this_zone_pad, ".tif")),
    #   gdal=c("COMPRESS=DEFLATE"),
    #   datatype = "FLT8S",
    #   overwrite = TRUE)
    
    write_csv(this_err_freq, 
              file.path(folder_qa,
                        paste0("unmatched_encoding_z", this_zone_pad, ".csv")))
    
    #with -1s still
    this_canopy_freq <- freq(this_canopy) %>% as_tibble()
    write_csv(this_canopy_freq, 
              file.path(folder_qa,
                        paste0("guide_results_z", this_zone_pad, ".csv")))
    
    
  } # end if save_unmatched
  
  #remove unmatched markers -- assign them CG value of 1
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
  # 3.4.2 Canopy guide toggle Modeled fire behavior is influenced by many vegetation attributes, but in forested areas, canopy is a particularly important factor affecting fire behavior outputs. A user-defined switch has been incorporated in LFTFC to describe the effects of canopy. The switch has the following three numeric settings.  • 0: This setting indicates no canopy on the site. Canopy Base Height (CBH) and Canopy Bulk Density (CBD) calculations from the LF data sets would be used to indicate the likelihood of crown fire on the site. • 1: This setting would be used on a tree life form site where torching and crowning in the upper strata vegetation is common during times of fire activity. • 2: This setting would be used on tree life form sites where torching and crowning are not likely to occur, such as in some hardwood communities. The mapping tool in this setting artificially raises CBH to 10 meters and lowers CBD to 0.012 kg/m³. Torching and crowning will not occur but the stand attributes (CH and CC) will be used in the calculations of surface fire behavior characteristics. The reason for keeping the canopy attributes in place is to adjust wind speed, shading, and sheltering for surface fire calculations. • 3: This setting would be used on tree life form sites where torching would occur, but active crowning is not likely, such as in mixed hardwood and conifer communities. The mapping tool in this setting artificially lowers CBD to 0.03 kg/m³, but leaves CBH, CH, and CC as predicted.
  this_canopy <- mask(this_canopy, this_canopy, maskvalues = -1)
  
  #disturbed pixels in the zone (safe 2 step)
  this_dist <- crop(dist, this_z_r)
  this_dist <- crop(this_dist, this_z_r, mask=TRUE)
  #set up base raster with CG = 1 in all disturbed pixels
  # (we will use the regression result update values where CG = 1)
  this_dist1 <- ifel(this_dist > 0, 1, NA)
  #Then put the rule-matched this_canopy on top on this. 
  #  I.e. replaces NA in this_canopy with value in this_dist1
  #  I.e. Only disturbed pixels, 
  #        with the value of the canopy guide from following the ruleset,
  #        with a default value of 1 (no applicable rule, etc.). 
  this_newcanopy <- cover(this_canopy, this_dist1)
  
  #In areas of high harvest (DIST codes 331, 332, 333), set CG to 0. 
  # This case was handled special as such in the python GEE code.  
  this_newcanopy <- ifel(this_dist %in% c(331, 332, 333), 0, this_newcanopy)
  
  #raster names
  names(this_newcanopy) <- "newCanopy"
  varnames(this_newcanopy) <- "newCanopy"
  
  # TODO - QA info from GEE python that we may want to translate over eventually
  # # create an image with information of what happened where
  # # if disturbed and has new value flag = 0
  # # if not distubed (i.e. initial 1 value) flag = 1
  # # if disturbed and has no remapped code flag = 2
  # # if outside of zone flag = 3
  # flags = (
  #   dist_img.Not() 
  #   .where(zone_newcanopy.add(1).mask().eq(0), 2) # .add(1) so 0 is no longer a valid value and can be masked
  #   .where(zone_img.neq(zone), 3)
  #   .updateMask(zone_img.mask())
  #   .uint8()
  #   .rename("qa_flags")
  # )
  # 
  
  # Save out
  writeRaster(
    this_newcanopy,
    file.path(folder_out, 
              paste0("canopyguide_z", this_zone_pad, ".tif")),
    gdal=c("COMPRESS=DEFLATE"))
  
}
(end_time <- Sys.time())
(end_time - start_time)


