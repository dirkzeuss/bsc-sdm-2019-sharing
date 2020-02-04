###  GIS Projekt Tagfalter Pakistan
##   Query raster data with vector data: some possibilities
##   Dirk Zeuss 
#    4.2.2020


## work in progress ##


#### Praeambel  ----------------------------------------------------------------------------------------------
library("raster")
library("rgdal")

# Clean workspace
rm(list=ls())

# Set working directory
wd <- "~/Schreibtisch/Projekt_Pakistan/GIS"
setwd(wd)

# Extent for computational region
ext <- extent(c(58, 83, 23, 38)) # in wgs 84, c(-4500000, 70000, -2500000, 3000000) in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 


#### Read in data  ----------------------------------------------------------------------------------------------

# Grid (or any other shapefile)
raster_50 <- readOGR("data/shape/Gitter/Gitter_50km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_50)) # Transforms to EPSG 102025

# Species richness map (or any other raster map)
richness_map <- raster("output/modelling/richness_map_bioclim.tif")

# Reality check
plot(richness_map)
plot(pakistan, add=TRUE)
plot(raster_50, add=TRUE)


##  Query raster data with vector data ----------------------------------------------------------------------------------------------------------------------

## if you have time or the data are small, use something like ----
raster_extract <- raster::extract(richness_map, raster_50, fun = function(x, na.rm=TRUE) mean(x)) # see ?extract() for details!
# or
raster_extract <- raster::extract(richness_map, raster_50) # see ?extract() for details!
result <- unlist(lapply(raster_extract, function(x) if (!is.null(x)) mean(x, na.rm=TRUE) else NA ))

## speed it up with multicore processing, try...
library("snow")

beginCluster(n=4)
raster_extract <- raster::extract(richness_map, raster_50, fun = function(x, na.rm=TRUE) mean(x)) # see ?extract() for details!
endCluster()


## if you have not so much time an do not care about using some fancy package, try ----
library("velox") # update the other packages first

vx_richness_map <- velox(richness_map) # make velox raster
velox_extract <- vx_richness_map$extract(raster_50) # see ?VeloxRaster_extract


## Or rasterize the vector and use the rasterized vector to query a raster layer (raster-raster query), try
rasterized_raster_50 <- rasterize(raster_50, richness_map, field=raster_50@data$id)


## or use the SAGA GIS bridge
library("RSAGA")
saga:gridstatisticsforpolygons


# ...OR... if nothing works, try standalone GRASS GIS or QGIS...



