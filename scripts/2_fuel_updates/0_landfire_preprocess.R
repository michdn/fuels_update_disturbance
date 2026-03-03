# Preprocess LANDFIRE data 

# 1. Encodes/Reclassifies FVH, FVT, FVC with short encoding ID
#    Also reclassifies -9999 to NA
# 2. Removes -1111, -9999 from FBFM40, canopy variables
# 3. BPS: Removes -9999, recodes -1111 to 8888
#         Extends BPS to later LF release extents

### Packages & Function -------------------------------

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  tidyverse,
  terra)

source(file.path("scripts", "common_vars_functions.R"))

### User settings -----------------------------------

source(file.path("scripts", "0_parameters", 
                 "2026_WRME_LF2024_updt2025.R"))

### Vegetation FV* -------------------------------------------

## Get rasters 
file_fvh <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVH_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fvh_orig <- rast(file_fvh)

file_fvc <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVC_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fvc_orig <- rast(file_fvc)

file_fvt <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FVT_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fvt_orig <- rast(file_fvt)


# Reclassifying FV: 
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

fvh_rcl <- read_csv(file_fvh_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvh_ufid) %>% 
  rename(IS = VALUE, BECOMES = fvh_ufid) %>% 
  as.matrix()

fvc_rcl <- read_csv(file_fvc_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvc_ufid) %>% 
  rename(IS = VALUE, BECOMES = fvc_ufid) %>% 
  as.matrix()

fvt_rcl <- read_csv(file_fvt_xwalk, show_col_types = FALSE) %>% 
  dplyr::select(VALUE, fvt_ufid) %>% 
  rename(IS = VALUE, BECOMES = fvt_ufid) %>% 
  as.matrix()

# Reclass / FV* encoding (first of the double encoding)
#others = -1 to see if any did not reclass correctly
fvh <- classify(fvh_orig, rcl = fvh_rcl, others = -1)
fvc <- classify(fvc_orig, rcl = fvc_rcl, others = -1)
fvt <- classify(fvt_orig, rcl = fvt_rcl, others = -1)

#rename 
varnames(fvh) <- paste0("LF", version_target, "_fvh_encoded")
names(fvh) <- paste0("LF", version_target, "_fvh_encoded")
varnames(fvc) <- paste0("LF", version_target, "_fvc_encoded")
names(fvc) <- paste0("LF", version_target, "_fvc_encoded")
varnames(fvt) <- paste0("LF", version_target, "_fvt_encoded")
names(fvt) <- paste0("LF", version_target, "_fvt_encoded")

#test to see if -1 has shown up, stop if so
fvh_min <- global(fvh, min, na.rm = TRUE)
fvc_min <- global(fvc, min, na.rm = TRUE)
fvt_min <- global(fvt, min, na.rm = TRUE)
# per 1. global() 2 min; min() 7 min; # app() almost 10 min
if (fvh_min == -1){stop("FVH classification failed some values.")}
if (fvc_min == -1){stop("FVC classification failed some values.")}
if (fvt_min == -1){stop("FVT classification failed some values.")}


## Save out 
writeRaster(fvc, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fvc_coded.tif")),
            gdal=c("COMPRESS=DEFLATE"))
writeRaster(fvh, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fvh_coded.tif")),
            gdal=c("COMPRESS=DEFLATE"))
writeRaster(fvt, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fvt_coded.tif")),
            gdal=c("COMPRESS=DEFLATE"))


## FBFM40 --------------------------------------------------------

file_fbfm40 <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_target, "_FBFM40_CONUS\\.tif$"),
  recursive = TRUE, 
  full.names = TRUE)
fbfm <- rast(file_fbfm40)

#Make -1111 and -9999 into NAs
fbfm <- mask(fbfm, fbfm, 
             maskvalues = c(-1111, -9999),
             updatevalue = NA)

writeRaster(fbfm, 
            file.path(folder_lfproc,
                      paste0("LF", version_target, "_fbfm.tif")),
            gdal=c("COMPRESS=DEFLATE"))


## Canopy fuels -----------------------------------------------------

#Use to apply rather than manual each time. 
# TODO: Could combine with FBFM40 if wanted, but would need to manage names differently
# ~1.6 hours (Chrome up, nothing else running)
canopy_vars <- c("CBD", "CBH", "CC", "CH")

process_canopy <- function(c_var){
  
  this_file <- list.files(
    path = folder_lfdata_base,
    pattern = paste0("^LF", version_target, "_", 
                     c_var, 
                     "_CONUS\\.tif$"),
    recursive = TRUE, 
    full.names = TRUE)
  
  this_rast <- rast(this_file)
  
  #Make -1111 and -9999 into NAs
  this_rast <- mask(this_rast, this_rast, 
                    maskvalues = c(-1111, -9999),
                    updatevalue = NA)
  
  
  writeRaster(this_rast, 
              file.path(folder_lfproc,
                        paste0("LF", version_target, "_", 
                               tolower(c_var), 
                               ".tif")),
              gdal=c("COMPRESS=DEFLATE"))
  
}
(start_time <- Sys.time())
#process each canopy variable
lapply(canopy_vars, process_canopy)
(end_time <- Sys.time())
(end_time - start_time)

## BPS LF 2020 -----------------------------------------------------

#BPS has older naming scheme
folder_bps <- list.files(
  path = folder_lfdata_base,
  pattern = paste0("^LF", version_bps, "_BPS_", "[0-9]*", "_CONUS$"),
  include.dirs = TRUE,
  full.names = TRUE)

file_bps <- list.files(
  path = file.path(folder_bps, "Tif"),
  pattern = paste0("\\.tif$"),
  full.names = TRUE)

bps <- rast(file_bps)
# IS ALLOWED TO HAVE -1111, there are rules for it, 
#   but need to recode for encoding criteria
#remove -9999
bps[bps==-9999] <- NA 
bps[bps==-1111] <- 8888

#Weird that extents changed between 2020 and 2024
# Should probably build an auto-check
bps_ext <- terra::extend(bps, fvh)

writeRaster(bps_ext, 
            file.path(folder_lfproc,
                      paste0("LF", version_bps, "_bps_coded.tif")),
            gdal=c("COMPRESS=DEFLATE"))


