  ###  GIS Projekt Tagfalter Pakistan
  ##   Methode: Bioclim
  ##   Dirk Zeuss 
  #    29.1.2020
  
  # This is just a rough template and MUST be adjusted!
  
  #### Praeambel  ----------------------------------------------------------------------------------------------
  library("raster")
  library("rgdal")
  library("dismo")
  
  
  # Clean workspace
  rm(list=ls())
  
  # Set working directory
  wd <- "/home/dirk/Schreibtisch/Projekt_Pakistan/"
  setwd(wd)
  
  # Extent for computational region
  ext <- extent(c(58, 83, 23, 38)) # in wgs 84, c(-4500000, 70000, -2500000, 3000000) in epsg 102025. Syntax: extent(length=4; order= xmin, xmax, ymin, ymax). 
  
  
  #### Read in and prepare data  ----------------------------------------------------------------------------------------------
  
  # Grid
  raster_50 <- readOGR("data/shape/Gitter/Gitter_50km.shp") # Read in grid. CRS is EPSG 102025 (Albers equal area). This is the project crs.
  
  # Country boundaries
  pakistan <- getData("GADM", country="Pakistan", level=0)
  pakistan <- spTransform(pakistan, crs(raster_50)) # Transforms to EPSG 102025
  
  # Distribution data  
  all_files_in_distribution     <- list.files(path = "data/distribution/Pakistan/", recursive = T) # List all files
  shp_paths                     <- grep(".shp$", all_files_in_distribution, value=TRUE) # Select shapefiles
  
  # Read in multiple shapefiles as elements of a list
   number_of_species_to_process <- 5 
  #number_of_species_to_process <- length(shp_paths) # All species
  
  shp_list <- list() 
    for(i in 1:number_of_species_to_process){ # Only number_of_testspecies for testing
       shp_list[[i]]                 <- readOGR(paste0("data/distribution/Pakistan/", shp_paths[i]))
       shp_list[[i]]                 <- spTransform(shp_list[[i]], crs(raster_50)) # Transforms to EPSG 102025
       shp_list[[i]]@data$species    <- gsub(".shp", "", basename(shp_paths[i]))
  }
  
  
  ## Prepare attribute tables of shapefiles as data.frames for modelling
  df_list <- list()
  for(i in 1:length(shp_list)){
    df_list[[i]]        <- as.data.frame(shp_list[[i]]) # transform to data.frame
    names(df_list[[i]]) <- gsub("coords.x1", "x", names(df_list[[i]])) # Adjust coordinate names
    names(df_list[[i]]) <- gsub("coords.x2", "y", names(df_list[[i]])) # Adjust coordinate names
  }
  
  
  ## Prepare environmental variables as predictors
  predictor_files <- list.files(path="data/raster/Worldclim/worldclim/", full.names=TRUE ) # Adjust file path to your personal working environment!
  
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
  
  # Checking if all layers make sense by visual inspection always is a good idea:
  # (be aware that the zoom function in RStudio can lead to strange non-overlapping results depending on the aspect ratio)
  
  plot(raster_50)
  plot(pakistan, add=TRUE)
  plot(predictors[[1]], add=TRUE)
  plot(shp_list[[4]], add=TRUE)
  
  
  ### Modelling ----------------------------------------------------------------------------------------------------------------------
  # Workflow: First, only one species then many species in a loop.
  # Objective is the creation of an area-wide presence/absence map.
  
  ## One species: ---
  bioclim_model <- bioclim(predictors, df_list[[3]][c("x", "y")]) # Number 3 is just one example species
  prediction <- dismo::predict(object = bioclim_model, x = predictors)
  
  # Reality check
  plot(prediction)
  plot(pakistan, add=TRUE)
  points(df_list[[3]][c("x", "y")])
  
  # Thresholding. This is crucial for the results. Here is one SUGGESTION which uses the third quantile as cut-off value
  threshold_bioclim <- raster::quantile(prediction)[4]
  
  ## Use the threshold for binary presence/absence classification of the prediction
  # Reclassification matrix for reclassify(). see ?raster::reclassify. Needs to have three columns. One row for each class: 1,10,999 becomes class 999 for values between 1 and 10, etc.
  classification_matrix <- matrix(c(0, threshold_bioclim, 0,
                                    threshold_bioclim, 1, 1),
                                  ncol=3, byrow = TRUE)
  
  # Reclassify
  result_presence_absence <- raster::reclassify(prediction, rcl = classification_matrix)
  
  plot(result_presence_absence)
  
  
  ## Now for many species: ---
  
  presence_absence_maps <- list()
  for(i in 1:length(df_list))try({
    cat(i, " ", df_list[[i]]$species[1], "\n")
    bioclim_model      <- bioclim(predictors, df_list[[i]][c("x", "y")])
    prediction         <- dismo::predict(object = bioclim_model, x = predictors)
    threshold_bioclim  <- raster::quantile(prediction)[4]
    classification_matrix <- matrix(c(0, threshold_bioclim, 0, threshold_bioclim, 1, 1),ncol=3, byrow = TRUE)
    result_presence_absence <- reclassify(prediction, rcl = classification_matrix)
    presence_absence_maps[[i]] <- result_presence_absence
    species_name <- df_list[[i]]$species[1]
    names(presence_absence_maps[[i]]) <- species_name # add species name to layer
    # Write out each modeled species as GeoTiff
    # If you don net yet have the directory, create it with e.g. dir.create(file.path(wd, "output/modelling/bioclim"), recursive=TRUE)
    writeRaster(presence_absence_maps[[i]], filename = file.path(wd, "output/modelling/bioclim", species_name), format="GTiff")
  })
  
  
  # Remove empty (null) entries. In this case those with only one record, which failed to be modelled with bioclim().
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
  writeRaster(richness_map, filename = file.path(wd, "output/modelling", "richness_map_bioclim"), format="GTiff")
  
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
  
  
  
  
  
  
  
