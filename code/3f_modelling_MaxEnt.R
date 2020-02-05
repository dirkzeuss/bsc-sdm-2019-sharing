###  GIS Projekt Tagfalter Pakistan
##   Methode: Maxent
##   Alexander Klug 
#    05.02.2020

#### Praeambel  ----------------------------------------------------------------------------------------------
library("raster")
library("rgdal")
library("dismo")


# Clean workspace
rm(list=ls())

# Set working directory
wd <- "D:/Kluga/GIS"
setwd(wd)

# Extent for computational region
ext <- extent(c(58, 83, 23, 38)) # in wgs 84, c(-4500000, 70000, -2500000, 3000000) in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 


#### Read in and prepare data  ----------------------------------------------------------------------------------------------

# Grid
raster_50 <- readOGR("../data/shape/Gitter/Gitter_50km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.

# Env_Layer
env_layer <- getData("worldclim", var = 'tmin', res = 10) # Just some worldclim-data, to get crs from Maxent-modelling

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_50)) # Transforms to EPSG 102025

### Modelling--Maxent-----------------------------------------------------------------------------------------------------------------------------

###########-------------------------------------------------------------------------###########
# Before one continues, you need to process Maxent extern, choose ../data/Output/Maxent_Output#
# for outputdirectory, so one can access the .asc files produced by Maxent in R.              #
###########-------------------------------------------------------------------------###########

# Distribution data (Maxent Output)

all_files_in_distribution   <- list.files(path = "../data/Output/Maxent_Output", recursive = T) # List all files
asc_paths                   <- grep(".asc$", all_files_in_distribution, value=TRUE)             # Select asciifiles

number_of_species_to_process <- length(asc_paths) # All species

asc_list <- list() 
for(i in 1:number_of_species_to_process){                                                         # Only number_of_testspecies for testing
  asc_list[[i]]                 <- raster(paste0("../data/Output/Maxent_Output/", asc_paths[i]))
  crs(asc_list[[i]])            <- crs(env_layer)                                                 #add crs back (lost in MaxEnt)
  asc_list[[i]]                 <- projectRaster(asc_list[[i]], crs = crs(pakistan))              #reproject to EPSG 102025
  species_name                  <- labels(asc_list[[i]])                                          #get species names for saving
  writeRaster(asc_list[[i]], filename = file.path(wd, "../data/Output/Maxent_Output_Tiff", species_name), format = "GTiff", overwrite = TRUE)
}

### Modelling--richness--map----------------------------------------------------------------------------------------------------------------------

## First for one species: --

# Reality check
plot(asc_list[[i]])
plot(pakistan, add=TRUE)

# Thresholding. This is crucial for the results. Here is one SUGGESTION which uses the third quantile as cut-off value
threshold_maxent <- raster::quantile(asc_list[[i]])[4]

## Use the threshold for binary presence/absence classification of the prediction
# Reclassification matrix for reclassify(). see ?raster::reclassify. Needs to have three columns. One row for each class: 1,10,999 becomes class 999 for values between 1 and 10, etc.
classification_matrix <- matrix(c(0, threshold_maxent, 0,
                                  threshold_maxent, 1, 1),
                                ncol=3, byrow = TRUE)

# Reclassify
result_presence_absence <- reclassify(asc_list[[i]], rcl = classification_matrix)

plot(result_presence_absence)

asc_list[[i]]



## Now for many species: ---

presence_absence_maps <- list()
for(i in 1:length(asc_list))try({
  threshold_maxent                   <- raster::quantile(asc_list[[i]])[4]
  classification_matrix              <- matrix(c(0, threshold_maxent, 0, threshold_maxent, 1, 1),ncol=3, byrow = TRUE)
  result_presence_absence            <- reclassify(asc_list[[i]], rcl = classification_matrix)
  presence_absence_maps[[i]]         <- result_presence_absence
  species_name                       <- labels(asc_list[[i]])
  names(presence_absence_maps[[i]])  <- species_name            # add species name to layer
  # Write out each modeled species as GeoTiff
  # If you don net yet have the directory, create it with e.g. dir.create(file.path(wd, "output/modelling/bioclim"), recursive=TRUE)
  writeRaster(presence_absence_maps[[i]], filename = file.path(wd, "../data/Output/presence_absence_maps", species_name), format="GTiff", overwrite = TRUE)
})


# Remove empty (null) entries. Normally they shouldnt exist, because MaxEnt removes all species' with less then five records.
presence_absence_maps <- presence_absence_maps[!unlist(lapply(presence_absence_maps, is.null))] 

# Create RasterStack from list
presence_absence_stack <- stack(presence_absence_maps) # transform list to RasterStack

# Sum over all RasterStack layers to create the final species richness map
richness_map <- stackApply(presence_absence_stack, indices=rep(1, nlayers(presence_absence_stack)), fun = sum, na.rm = TRUE)

# Plot result
plot(richness_map)
plot(pakistan, add=TRUE)


## Write out ----------------------------------------------------------------------------------------------------------------------

# GeoTiff
writeRaster(richness_map, filename = file.path(wd, "../data/Output/richness_maps", "richness_map_maxent"), format="GTiff", overwrite = TRUE)

# JPG
jpeg(filename = file.path(wd, "../data/Output/richness_maps", "richness_map_maxent.jpg"), width = 2000, height = 2000, quality = 99)
plot(richness_map)
plot(pakistan, add=TRUE)
dev.off()

# PDF
pdf(file.path(wd, "../data/Output/richness_maps", "richness_map_maxent.pdf"), width = 10, height = 10)
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







