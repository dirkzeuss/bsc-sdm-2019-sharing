###  GIS Projekt Tagfalter Pakistan
##   Methode: Random Forest
##   Mandy Gimpel nach Vorlage von Dirk Zeuss 
#    12.02.2020

#### Praeambel  ----------------------------------------------------------------------------------------------
library("raster")
library("rgdal")
library("dismo")
library("randomForest")

# Clean workspace
rm(list=ls())

# Set working directory
wd <- "D:.../Random_Forest/"
setwd(wd)

# Extent for computational region
ext <- extent(c(58, 83, 23, 38)) # in wgs 84, c(-4500000, 70000, -2500000, 3000000) in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 


#### Read in and prepare data  ----------------------------------------------------------------------------------------------

# Grid
raster_50 <- readOGR("Gitter/Gitter_50km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_50)) # Transforms to EPSG 102025

## Prepare environmental variables as predictors
predictor_files <- list.files(path="Predictors/", full.names=TRUE ) # Adjust file path to your personal working environment!

# Add more data here for better predictions!!!

# Read in environmental data files and transform crs to project crs
predictor_list <- list()
for(i in 1:length(predictor_files)){
  cat(predictor_files[i], "\n")
  predictor_list[[i]] <- raster(predictor_files[i]) # Read in single file
  predictor_list[[i]] <- projectRaster(predictor_list[[i]], crs=crs(raster_50)) # Reproject to project crs
}

predictors <- stack(predictor_list) # Transform list to RasterStack

butterflies_csv <- read.csv("PakistanLadakh points/PakistanLadak_points.csv", header=TRUE, sep=';') # read in points of butterflies

butterflies_csv_longlat <- SpatialPointsDataFrame(butterflies_csv[,c("x", "y")], 
                                                 butterflies_csv,
                                                 proj4string = crs(raster(predictor_files[1]))) #transform csv list into spatial points data frame

butterflies_csv_albers <- spTransform(butterflies_csv_longlat, crs(raster_50)) # transform crs so every spatial object is in the same crs (epsg 102025) 

butterflies_csv_albers <- as.data.frame(butterflies_csv_albers) # set as data frame

split_species <- split(butterflies_csv_albers , f = butterflies_csv_albers$species) # grouping data by species

## Now we have everything we need for modelling: 
# - A Grid system for counting species numbers in each cell later (raster_50), which also carries the project crs
# - Political boundaries of the study region for plotting and cropping of data (pakistan)
# - A RasterStack of predictor variables
# - Distribution data of multiple species in data.frames within a list (df_list)
# - Every spatial object in the same crs (epsg 102025)

# Checking if all layers make sense by visual inspection always is a good idea:
# (be aware that the zoom function in RStudio can lead to strange non-overlapping results depending on the aspect ratio)

plot(raster_50)
plot(predictors[[1]], add=TRUE)
plot(pakistan, add=TRUE)


#### Modelling ----------------------------------------------------------------------------------------------------------------------
# Workflow: First, only a few test-species then all species in a loop
# Objective is the creation of an area-wide presence/absence map.


# Loop first three species to test the loop
result <- list()
for(i in 1:3)try({
  cat(i, " ", as.character(split_species[[i]]$species[1]), "\n") # show i
  species <- split_species[[i]] #write one species in species
  
  #1 generating presence points
  presence_values <- extract(predictors, species[,c("x.1", "y.1")]) # Extracting presence values from rasters
  
  #2 generating absence points
  set.seed(123)
  absence_points <- randomPoints(predictors, nrow(species)) # creating random absence-points equal to number of presence points
  absence_values <- extract(predictors, absence_points) # Extracting absence values from rasters
  
  #3 putting pb and climate data into a single dataframe and set column 'pb' as factor, otherwise it'll do Regression
  pb <- c(rep(1, nrow(presence_values)), rep(0, nrow(absence_values))) 
  sdmdata <- data.frame(cbind(pb, rbind(presence_values, absence_values)))
  sdmdata[,'pb'] = as.factor(sdmdata[,'pb'])

  #4 generating Random Forest
  set.seed(123) 
  myForest<- randomForest(pb ~., data=sdmdata, importance=TRUE, mtry=3, ntree=300, replace=TRUE, do.trace=TRUE, keep.forest=TRUE, na.action=na.roughfix)
  myForest
  
  #5 output
  species_name <- as.character(species$species[1])
  prediction <- predict(predictors, myForest)

  result[[i]] <- prediction

  #6 writing maps for every species
  writeRaster(prediction, filename = file.path(wd, "Output/Single_Species/", species_name), format="GTiff")
})

# creating a Richness Map with Test-Species Data
richness_files <- list.files(path="Output/Single_Species/", full.names=TRUE ) # Adjust file path to your personal working environment!
richness_list <- list()

for(i in 1:length(richness_files)){
  cat(richness_files[i], "\n")
  richness_list[[i]] <- raster(richness_files[i]) # Read in single file
}  

# Create RasterStack from list
richness_stack <- stack(richness_list) # Transform list to RasterStack  
  
# Sum over all RasterStack layers to create the final species richness map
richness_map <- stackApply(richness_stack, indices=rep(1, nlayers(richness_stack)), fun = sum, na.rm = TRUE)

# Plot result
plot(richness_map)
plot(pakistan, add=TRUE)

# Output GeoTiff
writeRaster(richness_map, filename = file.path(wd, "Output/Richness_Map/", "richness_map_random_forest"), format="GTiff")


# Loop for all Species
result <- list()
for(i in 1:length(split_species))try({
  cat(i, " ", as.character(split_species[[i]]$species[1]), "\n") # show i
  species <- split_species[[i]] #write one species in species
  
  #1 generating presence points
  presence_values <- extract(predictors, species[,c("x.1", "y.1")]) # Extracting presence values from rasters
  
  #2 generating absence points
  set.seed(123)
  absence_points <- randomPoints(predictors, nrow(species)) # creating random absence-points equal to number of presence points
  absence_values <- extract(predictors, absence_points) # Extracting absence values from rasters
  
  #3 putting pb and climate data into a single dataframe and set column 'pb' as factor, otherwise it'll do Regression
  pb <- c(rep(1, nrow(presence_values)), rep(0, nrow(absence_values))) 
  sdmdata <- data.frame(cbind(pb, rbind(presence_values, absence_values)))
  sdmdata[,'pb'] = as.factor(sdmdata[,'pb'])
  
  #4 generating Random Forest
  set.seed(123) 
  myForest<- randomForest(pb ~., data=sdmdata, importance=TRUE, mtry=3, ntree=300, replace=TRUE, do.trace=TRUE, keep.forest=TRUE, na.action=na.roughfix)
  myForest

  #5 output
  species_name <- as.character(species$species[1])
  prediction <- predict(predictors, myForest)

  result[[i]] <- prediction
  
  # writing maps for every species
  writeRaster(prediction, filename = file.path(wd, "Output/Single_Species/", species_name), format="GTiff")
})

# creating a Richness Map with all species Data
richness_files <- list.files(path="Output/Single_Species/", full.names=TRUE ) # Adjust file path to your personal working environment!
richness_list <- list()

for(i in 1:length(richness_files)){
  cat(richness_files[i], "\n")
  richness_list[[i]] <- raster(richness_files[i]) # Read in single file
}  

# Create RasterStack from list
richness_stack <- stack(richness_list) # Transform list to RasterStack  


# Sum over all RasterStack layers to create the final species richness map

# caution: it may take a while. On my pc it took between 18 and 24 hours to calculate the sum with the whole stack of 400+ files.
# Meanwhile I took another turn and split the file-stack into smaller parts to process them faster. 
# Processing only 100 files at a time and summing them up afterwards was much faster. With this method it took only 1.5 hours to process all 400+ files.
# But if you take this method remember to edit the value 1 (which is the value to be summed) to !0 (not zero), 
# because the value 1 is no longer present due to the previous step of summing up.

richness_map <- stackApply(richness_stack, indices=rep(1, nlayers(richness_stack)), fun = sum, na.rm = TRUE) # with all 400+ files
#richness_map <- stackApply(richness_stack, indices=rep(!0, nlayers(richness_stack)), fun = sum, na.rm = TRUE) # with intermediate steps

# Plot result
plot(richness_map)
plot(pakistan, add=TRUE)

# Output GeoTiff
writeRaster(richness_map, filename = file.path(wd, "Output/Richness_Map/", "richness_map_random_forest"), format="GTiff")







