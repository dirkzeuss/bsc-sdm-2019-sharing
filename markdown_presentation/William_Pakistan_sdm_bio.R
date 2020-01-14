# SDM Pakistan - Bioclim #



# Distribution data preparation

install.packages("shapefiles")

library(sp)
library(dismo)
library(raster)
library(gbm)
library(maptools)
library(rgdal)
library(shapefiles)

vignette('sdm', 'dismo')
data("wrld_simpl")


###Data distribution

# read Shapefile and create data.frame

dis_shape <- read.shp("H:/Universität Geographie/Projekt/dis_pak/data/distribution_Pakistan_all_test.shp")

dis_dataframe <- as.data.frame(dis_shape)



# create .csv data

write.csv(dis_shape, file="H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")

occurrence_csv <- paste0(system.file(package="dismo"), "H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")

occurrence <- read.csv("H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")



# let only appear x and y columns 

occurrence_lonlat <- occurrence [,3:4]

head(occurrence_lonlat)

write.csv(occurrence_lonlat, "H:/Universität Geographie/Projekt/dis_pak/data/distribution.csv")







### worldclim

worldclim_predictors <- getData("worldclim",var="bio",res=10)

worldclim_predictors

worldclim_predictors <- worldclim_predictors[[c(1,12)]]

names(worldclim_predictors) <- c("Temp","Prec")

names(worldclim_predictors)

values_worldclim_pred <- extract(worldclim_predictors, occurrence_lonlat)

df_occurrence_worldclim <- cbind.data.frame(coordinates(occurrence_lonlat), values_worldclim_pred)

head(df_occurrence_worldclim)



# plot worldclim data

plot(worldclim_predictors[[1]])
plot(df_occurrence_worldclim,add=T)
plot(df_occurrence_worldclim)
plot(values_worldclim_pred)
plot(occurrence_lonlat)
plot(worldclim_predictors[[1]], points(occurrence_lonlat) , add=T)
plot(df_occurrence_worldclim)
plot(occurrence_lonlat,worldclim_predictors)



# Borders 

# get data

data(wrld_simpl)

wrld_simpl



# plot borders occurence_lotlat

plot(occurrence_lonlat)

plot(wrld_simpl, add=T, border='yellow', lwd=2)

str(dis_dataframe)

head(dis_dataframe)



### chelsa data preparation 

# create path and file

merged_chelsa_path <- "H:/Universität Geographie/Projekt/dis_pak/chelsa/"

merged_chelsa_path


merged_chelsa_files <- list.files("H:/Universität Geographie/Projekt/dis_pak/chelsa/")

merged_chelsa_files



# merge the chelsa data 



annual_mean_temp <- raster("H:/Universität Geographie/Projekt/dis_pak/chelsa/annual_mean_T.tif")

annual_global_prec <- raster("H:/Universität Geographie/Projekt/dis_pak/chelsa/CHELSA_bio10_12.tif")

prec_warmest_quarter <- raster("H:/Universität Geographie/Projekt/dis_pak/chelsa/CHELSA_bio10_18.tif")

warmest_quarter_mean_temp <- raster("H:/Universität Geographie/Projekt/dis_pak/chelsa/CHELSA_bio10_10.tif")


raster_list <- list(all_chelsa_data) 
raster_list
for(i in 1:12)
{
  tilesloop <- paste0(annual_mean_temp, annual_global_prec, prec_warmest_quarter, warmest_quarter_mean_temp[i])
  tilesloop       
  raster_list[[i]] <- raster(tilesloop)
  rasterliste <- raster_list[[i]]
  rasterliste
}

raster_list$filename     <- "H:/Universität Geographie/Projekt/dis_pak/chelsa/..."
exporttif <- raster_list$filename 
exporttif
raster_list$overwrite    <- TRUE


all_merged_chelsa_annual_mean_temp <- do.call(raster::merge, raster_list, quote=TRUE)



# plot chelsa data

plot(annual_mean_temp, main="annual mean temperature")
plot(annual_global_prec, main="annual global prec")
plot(prec_warmest_quarter, main="warmest quarter prec")
plot(warmest_quarter_mean_temp, main="warmest quarter mean temperature")



# crop and plot merged chelsa data 

extend_pakistan <- extent(c(58, 83, 23, 38))

cropped_annual_mean_temp <- crop(annual_mean_temp, extend_pakistan)
plot(cropped_chelsa_annual_mean_temp)

cropped_annual_global_prec <- crop(annual_global_prec,extend_pakistan)
plot(cropped_chelsa_annual_global_prec)

cropped_prec_warmest_quarter <- crop(prec_warmest_quarter, extend_pakistan)
plot(cropped_chelsa_prec_warmest_quarter)

cropped_warmest_quarter_mean_temp <- crop(warmest_quarter_mean_temp, extend_pakistan,)
plot(cropped_chelsa_warmest_quarter_mean_temp)



# create rasterstack with cropped chelsa data and plot it 

rasterstack_pak_chelsa <- raster::stack(cropped_annual_global_prec, cropped_annual_mean_temp, cropped_prec_warmest_quarter, cropped_warmest_quarter_mean_temp)
plot(rasterstack_pak_chelsa)



# get data.frame of chelsa

pak_chelsa_dataframe <- as.data.frame(rasterstack_pak_chelsa)
head(pak_chelsa_dataframe)



# create dataframe of distribution data, .csv

distribution_shape <- read.shp("H:/Universität Geographie/Projekt/dis_pak/data/distribution_Pakistan_all_test.shp")
distribution_dataframe <- as.data.frame(distribution_shape)

str(distribution_dataframe)

write.csv(distribution_shape, file = "H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")
head(distribution_shape)

occurrence_dis <- paste0(system.file(package="dismo"), "H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")
head(occurrence_dis)


occurrence <- read.csv("H:/Universität Geographie/Projekt/dis_pak/data/distribution_shape_csv.csv")
head(occurrence)

df_occurrence_lonlat <- occurrence [,2:4]
head(df_occurrence_lonlat)

write.csv(df_occurrence_lonlat, "H:/Universität Geographie/Projekt/dis_pak/data/df_occurrence_lonlat.csv")
head(df_occurrence_lonlat)




# combine 'data.frame distribution data' and 'chelsa data'

combined_dis_chelsa_df <- data.frame(dis_shape, rasterstack_pak_chelsa)



## Species distribution data preparation

# read species.csv

table_species_subspecies <- read.csv("H:/Universität Geographie/Projekt/dis_pak/data/distribution_Pakistan_all_species.csv")

species_subspecies <- species_subspecies [,0:2]
head(species_subspecies)



# Combine data of species_subspecies and df_occurrence_lonlat in data.frame

combi_occ_species <- data.frame(df_occurrence_lonlat, species_subspecies)
head(combi_occ_species)



# Extract raster values from Points

raster_value_pak <- extract(rasterstack_pak_chelsa, occurrence_lonlat)

raster_value_pak



# dataframe of all data

pakistan_butterflies_train <- data.frame(df_occurrence_lonlat, species_subspecies, raster_value_pak)
head(pakistan_butterflies_train)




### bioclim

bc <- bioclim(pred_nf, pres_train)
plot(bc, a=1, b=2, p=0.85)


predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern = 'grd$', full.names = TRUE))

file <- file.path(system.file(package = "dismo"), "ex/distribution_Pakistan_all_test.shp")
file


distribution <- read.table(file, header = TRUE, sep=',')
presvals <- extract(predictors, bradypus)
presvals
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmata[,'biome'] <- as.factor(sdmata[,'biome'])





