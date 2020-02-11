library("raster")
library("rgdal")
library("dismo")
library("dplyr")

# Clean workspace
rm(list=ls())

# Set working directory
wd <- "E:/Uni/5. Semester/Pakistan_Projekt/Butterflys_map_GLM"
setwd(wd)

set.seed(1)

# Extent for computational region
ext <- extent(c(58, 83, 23, 38)) # in wgs 84, c(-4500000, 70000, -2500000, 3000000) in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 


#### Read in and prepare data  ----------------------------------------------------------------------------------------------

# Grid
raster_50 <- readOGR("E:/Uni/5. Semester/Pakistan_Projekt/bsc-sdm-2019/data/Gitter/Gitter_25km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_50)) # Transforms to EPSG 102025


## When just a csv file is availible
all_species_tab <- read.csv("E:/Uni/5. Semester/Pakistan_Projekt/bsc-sdm-2019/data/distribution/PakistanLadakh/PakistanLadak.csv", sep = ";")


number_of_species_to_process <- nrow(all_species_tab)
unique <- unique(all_species_tab$species) # seperates just the names of the species
unique[[1]]

#splites the dataframe into a list with dataframes for every species
splited_df <- split(all_species_tab, all_species_tab$species)
str(splited_df)

# Now we create a list with a dataframe for every species with just the coordinates in it
cor_list <- list()
for(i in 1:length(splited_df)){
  cor_list[[i]] <- data.frame(splited_df[[i]]$coords.x1,splited_df[[i]]$coords.x2)
  colnames(cor_list[[i]]) =  c('x','y')
}
cor_list

## Prepare environmental variables as predictors
predictor_files <- list.files(path="E:/Uni/5. Semester/Pakistan_Projekt/Daten/Raster/Worldclim/",pattern = ".tif", full.names=TRUE ) # Adjust file path to your personal working environment!

# Add more data here for better predictions!!!

# Read in environmental data files and transform crs to project crs
predictor_list <- list()
for(i in 1:length(predictor_files)){
  cat(predictor_files[i], "\n")
  predictor_list[[i]] <- raster(predictor_files[i]) # Read in single file
  predictor_list[[i]] <- projectRaster(predictor_list[[i]], crs=crs(raster_50)) # Reproject to project crs
}


predictors <- stack(predictor_list) # Transform list to RasterStack

predictors <- dropLayer(predictors, 'biome') # Remove this layer because it is not metric

## Now we have everything we need for modelling: 
# - A Grid system for counting species numbers in each cell later (raster_50), which also carries the project crs
# - Political boundaries of the study region for plotting and cropping of data (pakistan)
# - Distribution data of multiple species in data.frames within a list (df_list)
# - A RasterStack of predictor variables
# - Every spatial object in the same crs (epsg 102025)


plot(raster_50)
plot(pakistan, add=TRUE)
plot(predictors[[1]], add=TRUE)
# plot(shp_list[[2]], add=TRUE)

# for a generalize linear model we need absent and presence points

backgr <- list()
presvals <- list()
absvals <- list()
for(i in 1:length(cor_list)){
  backgr[[i]]   <-  randomPoints(predictors, nrow(cor_list[[i]]), ext = pakistan) # create random points
  presvals[[i]] <-  extract(predictors, cor_list[[i]]) # creates a list of dataframes with the values for every present point
  absvals[[i]]  <-  extract(predictors, backgr[[i]])   # creates a list of dataframes with the values for every absent point
}
##################################################
cor_list[[1]]
backgr[[1]]
presvals[[1]]
absvals[[1]]



pb      <- list()
sdmdata <- list()
for(i in 1:length(presvals)){
  pb[[i]] <- c(rep(1, nrow(presvals[[i]])), rep(0, nrow(absvals[[i]]))) 
  sdmdata[[i]] <- data.frame(cbind(pb[[i]], rbind(presvals[[i]], absvals[[i]]))) # create dataframes with the values of every point and one colum with 1 for present and 0 for absent points
}
pb[[1]]
sdmdata[[1]]

group <- list()
pres_train <- list()
pres_test <- list()

for(i in 1:length(cor_list))try({
  group[[i]] <- kfold(cor_list[[i]], 3)
  pres_train[[i]] <- cor_list[[i]][group[[i]] != 1, ]
  pres_test[[i]] <- cor_list[[i]][group[[i]] == 1, ]
})
pres_train[[1]]
backg <- list()
backg_train <- list()
backg_test <- list()

for(i in 1 : length(cor_list))try({
  backg[[i]] <- randomPoints(predictors, nrow(cor_list[[i]]), ext = pakistan)
  colnames(backg[[i]]) = c('x','y')
  group[[i]] <- kfold(backg[[i]], 3)
  backg_train[[i]] <- backg[[i]][group[[i]] != 1, ]
  backg_test[[i]] <- backg[[i]][group[[i]] == 1, ]
})


str(backg_test)
backg_train[[2]]

train <- list()
pb_train <- list()
envtrain <- list()


for(i in 1 : length(cor_list))try({
  train[[i]]    <- rbind(pres_train[[i]], backg_train[[i]])
  pb_train[[i]] <- c(rep(1, nrow(pres_train[[i]])), rep(0, nrow(backg_train[[i]])))
  envtrain[[i]] <- extract(predictors, train[[i]])
  envtrain[[i]] <- data.frame(cbind(pa=pb_train[[i]], envtrain[[i]]))
})

pb_train[[1]]
head(envtrain) #shows the head of the list


#The same for the test data
testpres <- list()
testbackg <- list()

for(i in 1 : length(cor_list))try({
  testpres[[i]] <- data.frame( extract(predictors, pres_test[[i]]) )
  testbackg[[i]] <- data.frame( extract(predictors, backg_test[[i]]) )
})


testpres[[1]]
testbackg[[1]]
envtrain[[2]]

### Modelling ----------------------------------------------------------------------------------------------------------------------
# Workflow: First, only one species then many species in a loop.
# Objective is the creation of an area-wide presence/absence map.
glm_pak <- list()
for(i in 1 : length(envtrain)){
  glm_pak[[i]] <- glm(pa ~ .,family = binomial(link = "logit"), data=envtrain[[i]])
}

summary(glm_pak[[6]])


prediction <- raster::predict( predictors, glm_pak[[6]], ext=pakistan)


prediction
minValue(prediction)
# Reality check
plot(prediction)
plot(pakistan, add=TRUE)
points(cor_list[[6]][c("x", "y")])

# Thresholding. This is crucial for the results. Here is one SUGGESTION which uses the third quantile as cut-off value
#threshold_glm <- raster::quantile(prediction)[4]

## Use the threshold for binary presence/absence classification of the prediction
# Reclassification matrix for reclassify(). see ?raster::reclassify. Needs to have three columns. One row for each class: 1,10,999 becomes class 999 for values between 1 and 10, etc.
#classification_matrix <- matrix(c(minValue(prediction),threshold_glm , 0,
                                  threshold_glm, maxValue(prediction), 1),
                                ncol=3, byrow = TRUE)
#classification_matrix
## Reclassify
#result_presence_absence <- reclassify(prediction, rcl = classification_matrix)

#plot(result_presence_absence)
#writeRaster(result_presence_absence, filename = file.path( "E:/Uni/5. Semester/Pakistan_Projekt/Daten/GLM_Pak_output", unique[[i]]), format="GTiff")

## Now for many species: ---

presence_absence_maps <- list()
for(i in 1:length(envtrain))try({
  cat(i, " ", unique[[i]], "\n")
  glm_pak <- glm(pa ~ .,family = binomial(link = "logit"), data=envtrain[[i]])
  prediction <- raster::predict( predictors, glm_pak, ext=pakistan)
  threshold_glm <- raster::quantile(prediction)[4]
  classification_matrix <- if(minValue(prediction)<0){
                                    matrix(c(minValue(prediction),threshold_glm , 0,
                                    threshold_glm, maxValue(prediction), 1),
                                    ncol=3, byrow = TRUE)
                                    }else
                                    {matrix(c(minValue(0, threshold_glm), 0,
                                            threshold_glm, maxValue(prediction), 1),
                                            ncol=3, byrow = TRUE)}
  result_presence_absence <- reclassify(prediction, rcl = classification_matrix)
  values(result_presence_absence)[values(result_presence_absence) < 0] = NA
  presence_absence_maps <- result_presence_absence
  
  # add species name to layer
  # Write out each modeled species as GeoTiff
  # If you don net yet have the directory, create it with e.g. dir.create(file.path(wd, "output/modelling/bioclim"), recursive=TRUE)
  writeRaster(presence_absence_maps, filename = file.path("E:/Uni/5. Semester/Pakistan_Projekt/Daten/GLM_output2", unique[[i]]), format="GTiff", overwrite=TRUE)
})

envtrain[[1]]
all_maps <- list.files("E:/Uni/5. Semester/Pakistan_Projekt/Daten/GLM_output2/", pattern = ".tif", full.names = TRUE)
all_maps[[1]]

# read in as rasterlayer
all_maps_as_raster <- list()
for(i in 1:length(all_maps)){
  all_maps_as_raster[[i]] <- raster(all_maps[[i]])
}


# Create RasterStack from list, transform list to RasterStack
maps_merged <- raster::stack(all_maps_as_raster)

# Sum over all RasterStack layers to create the final species richness map
richness_map <- stackApply(maps_merged, indices=rep(1, nlayers(maps_merged)), fun = sum, na.rm = TRUE)



richness_map_croped <- crop(richness_map,pakistan)

# Plot result
plot(richness_map)
plot(pakistan, add=TRUE)
writeRaster(richness_map_croped, filename = file.path("E:/Uni/5. Semester/Pakistan_Projekt/Daten/richness_map/", "richness_map_glm_croped"), format="GTiff")

library(maptools)
Pakistan <- writeSpatialShape(pakistan, ".shp")
writeOGR(Pakistan, dsn = "E:/Uni/5. Semester/Pakistan_Projekt/Daten/richness_map/" ,layer = "Pakistan", driver = "ESRI Shapefile")
## Write out ----------------------------------------------------------------------------------------------------------------------

# GeoTiff


# JPG
jpeg(filename = file.path(wd, "output/modelling", "richness_map_bioclim.jpg"), width = 2000, height = 2000, quality = 99)
plot(richness_map)
plot(pakistan, add=TRUE)
dev.off()

# PDF
pdf(file.path(wd, "output/modelling", "richness_map_bioclim.pdf"), width = 10, height = 10)
plot(richness_map)
plot(pakistan, add=TRUE)
dev.off()



## Chunks   ----------------------------------------------------------------------------------------------------------------------

# For other methods which require VALUES of environmental data at presence locations, 
# or VALUES of environmental data at pseudo absence locations, the following chuncks may be useful:

# Extract environmental information at PRESENCE locations of a/multiple species
# presence_values <- extract(predictors, df_list[[3]][c("x", "y")])

# Set random background points for extracting environmental information of pseudo absences (we do not have real absence values)
# background_points <- randomPoints(predictors, nrow(df_list[[3]])) # the amount of background points in this example is equal to the number of presence points

# Extract environmental information at ABSENCE locations of a/multiple species
# absence_values <- extract(predictors, background_points)
