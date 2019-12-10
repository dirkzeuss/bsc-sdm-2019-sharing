###  GIS Projekt Tagfalter Pakistan
##   
##   Dirk Zeuss
#    8.4.2019

#### Praeambel  ####
library("raster")

rm(list=ls())

# setwd
wd <- "~/Arbeitsfläche/Projekt_Pakistan/GIS/"
setwd(wd)


# extent computational region

ext <- extent(c(58, 83, 23, 38)) #vector (length=4; order= xmin, xmax, ymin, ymax) # c(-4500000, 70000, -2500000, 3000000) in epsg 102025




###  SRTM  ------------------------------------------------------------------------------------------------------

# read in

path_SRTM_tiles <- paste0(wd, "data/SRTM_Asia/")

files_SRTM_tiles <- list.files(path_SRTM_tiles)

files_SRTM_tiles <- files_SRTM_tiles


raster_list <- list() 
for(i in 1:length(files_SRTM_tiles)){
     raster_list[[i]] <- raster(paste0(path_SRTM_tiles, files_SRTM_tiles[i]))
}


raster_list$tolerance    <- 1
raster_list$filename     <- paste(wd, "output/", "SRTM_merge.tif", sep = "")
raster_list$overwrite    <- TRUE
# raster_list$ext          <- ext                                                     #extent


all_merged <- do.call(raster::merge, raster_list, quote=T)

SRTM_cropped <- crop(all_merged, ext)


writeRaster(SRTM_cropped, paste0(wd, "output/", "SRTM_cropped.tif"))

writeRaster(all_merged, paste0(wd, "output/", "SRTM_all.tif"))





### Rest -------------------------------------------------------------------------------------------


