###  GIS Projekt Tagfalter Pakistan
##   Methode: Bioclim
##   Dirk Zeuss 
#    29.1.2020

# This is just a rough template and MUST be adjusted!

#### Praeambel  ----------------------------------------------------------------------------------------------
library("raster")
library("rgdal")
library("dismo")
library("e1071")


# Clean workspace
rm(list=ls())

# Set working directory
wd <- "T:/Projekt_Pakistan/bsc-sdm-2010-hessenbox"
setwd(wd)


# Extent for computational region
ext <- extent(c(-4500000, 70000, -2500000, 3000000)) #in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 


#### Read in and prepare data  ----------------------------------------------------------------------------------------------

# Grid
raster_25 <- readOGR("T:/Projekt_Pakistan/bsc-sdm-2010-hessenbox/Gitter/Gitter_25km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_25)) # Transforms to EPSG 102025

# Distribution data  
#all_files_in_distribution     <- list.files(path = "../data/distribution/Pakistan/", recursive = T) # List all files
#shp_paths                     <- grep(".shp$", all_files_in_distribution, value=TRUE) # Select shapefiles

# Read in multiple shapefiles as elements of a list
# number_of_species_to_process <- 5 
# number_of_species_to_process <- length(shp_paths) # All species

#shp_list <- list() 
#  for(i in 1:number_of_species_to_process){ # Only number_of_testspecies for testing
#     shp_list[[i]]                 <- readOGR(paste0("../data/distribution/Pakistan/", shp_paths[i]))
#     shp_list[[i]]                 <- spTransform(shp_list[[i]], crs(raster_50)) # Transforms to EPSG 102025
#     shp_list[[i]]@data$species    <- gsub(".shp", "", basename(shp_paths[i]))
#}
  
#Import & project point data set of butterflies
all_spec<- read.csv("T:/Projekt_Pakistan/bsc-sdm-2010-hessenbox/PakistanLadak2.csv", sep = ",")

coordinates(all_spec) <- ~ coords.x1 + coords.x2

crs(all_spec) <- "+proj=longlat +datum=WGS84 +no_defs"

all_spec <- spTransform(all_spec, CRS("+init=epsg:4326"))

all_spec <- spTransform(all_spec, crs(raster_25))

spec_list <- split(all_spec, all_spec$species) #create list from all_spec.csv


## Prepare attribute tables of shapefiles as data.frames for modelling

df_list <- list()
for(i in 1:length(spec_list)){
  df_list[[i]]        <- as.data.frame(spec_list[[i]]) # transform to data.frame
  names(df_list[[i]]) <- gsub("coords.x1", "x", names(df_list[[i]])) # Adjust coordinate names
  names(df_list[[i]]) <- gsub("coords.x2", "y", names(df_list[[i]])) # Adjust coordinate names
}

## Prepare environmental variables as predictors
predictor_files <- list.files(path="T:/Projekt_Pakistan/bsc-sdm-2010-hessenbox/worldclim", full.names=TRUE) # Adjust file path to your personal working environment!

# Add more data here for better predictions!!!

# Read in environmental data files and transform crs to project crs
predictor_list <- list()
for(i in 1:length(predictor_files)){
 cat(predictor_files[i], "\n")
 predictor_list[[i]] <- raster(predictor_files[i]) # Read in single file
 predictor_list[[i]] <- projectRaster(predictor_list[[i]], crs=crs(raster_25)) # Reproject to project crs
}

predictors <- stack(predictor_list) # Transform list to RasterStack

predictors <- dropLayer(predictors, 'biome') # Remove this layer because it is not metric



## Now we have everything we need for modelling: 
# - A Grid system for counting species numbers in each cell later (raster_50), which also carries the project crs
# - Political boundaries of the study region for plotting and cropping of data (pakistan)
# - Distribution data of multiple species in data.frames within a list (df_list)
# - A RasterStack of predictor variables
# - Every spatial object in the same crs (epsg 102025)

# Checking if all layers make sense by visual inspection always is a good idea:
# (be aware that the zoom function in RStudio can lead to strange non-overlapping results depending on the aspect ratio)

plot(raster_25)
plot(pakistan)
plot(predictors[[1]], add=TRUE)
plot(all_spec, add=TRUE)

### Modelling ----------------------------------------------------------------------------------------------------------------------
# Workflow: First, only one species then many species in a loop.
# Objective is the creation of an area-wide presence/absence map.

## One species: ---
library("sdm")
library("usdm")

# Remove Species with <2 records
df_list[[89]] <- NULL 
df_list[[181]] <- NULL
df_list[[267]] <- NULL #Paralasa_afghana
df_list[[273]] <- NULL #Parantica_sita
df_list[[273]] <- NULL #Pareronia_anais
df_list[[280]] <- NULL #Parnassius_elegans
df_list[[282]] <- NULL #Parnassius_inopinatus
df_list[[311]] <- NULL #Plebejus_leela
df_list[[338]] <- NULL #Polyommatus_venus
df_list[[350]] <- NULL #Pseudochazara_annieae
df_list[[356]] <- NULL #Pyrgus_badachschanus
df_list[[368]] <- NULL #Seseria_dohertyi
df_list[[378]] <- NULL #Spindasis_schistacea
df_list[[398]] <- NULL #Turanana_kotzschiorum
df_list[[400]] <- NULL #Vagrans_egista
df_list[[270]] <- NULL #Paralasa_kotzschae
df_list[[282]] <- NULL #Parnassius_loxias
df_list[[397]] <- NULL #Udara_dilecta

# Create GTiff

result <- list()
for (i in 353:length(df_list))({
  cat(i, " ", df_list[[i]]$species[1], "\n")
  a <- df_list[[150]][c("x", "y")]
  a$presence <- 1
  coordinates(a) <- ~x+y
  svm_model <- sdmData(presence ~., a, predictors = predictors, bg = list(n=1000))
  svm_model <- sdm(presence~., svm_model, methods=c('svm'))
  svm_model <- predict(svm_model, predictors)
  threshold_svm <- raster::quantile(svm_model)[4]
  classification_matrix <- matrix(c(0, threshold_svm, 0, threshold_svm, 1, 1),ncol=3, byrow = TRUE)
  result_presence_absence <- reclassify(svm_model, rcl = classification_matrix)
  result[[i]] <- result_presence_absence
  species_name <- df_list[[i]]$species[1] # add species name to layer
  writeRaster(result[[i]], filename = file.path(wd, "GIS/output/modelling/svm", species_name), format="GTiff", overwrite = TRUE)
})


# Remove empty (null) entries. In this case those with only one record, which failed to be modelled with bioclim().
presence_absence_maps <- result[!unlist(lapply(result, is.null))] 

# Create RasterStack from list
presence_absence_stack <- stack(presence_absence_maps) # transform list to RasterStack

# Sum over all RasterStack layers to create the final species richness map
richness_map <- stackApply(presence_absence_stack, indices=rep(1, nlayers(presence_absence_stack)), fun = sum, na.rm = TRUE)

# Plot result
plot(richness_map)
plot(pakistan, add=TRUE)


## Write out ----------------------------------------------------------------------------------------------------------------------

# GeoTiff
writeRaster(richness_map, filename = file.path(wd, "GIS/output/modelling", "richness_map_svm"), format="GTiff", overwrite = TRUE)

# JPG
jpeg(filename = file.path(wd, "GIS/output/modelling", "richness_map_svm.jpg"), width = 2000, height = 2000, quality = 99)
plot(richness_map)
plot(pakistan, add=TRUE)
dev.off()

# PDF
pdf(file.path(wd, "GIS/output/modelling", "richness_map_svm.pdf"), width = 10, height = 10)
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







