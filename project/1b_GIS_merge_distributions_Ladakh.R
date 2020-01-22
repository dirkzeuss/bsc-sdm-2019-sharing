###  GIS Projekt Tagfalter Pakistan
##   
##   Dirk Zeuss
#    8.4.2019

# Anpassen an Ladakh Daten!!

#### Praeambel  ####
library("raster")
library("rgdal")
library("maptools")
library("geonames")

rm(list=ls())

options(max.print=6000)

# setwd
wd <- "~/Schreibtisch/Projekt_Pakistan/GIS/"
setwd(wd)


# Extent for computational region
ext <- extent(c(58, 83, 23, 38)) #vector (length=4; order= xmin, xmax, ymin, ymax) # c(-4500000, 70000, -2500000, 3000000) in epsg 102025


# Read in GIS data  ----------------------------------------------------------------------------------------------


# Gitter
raster_50 <- readOGR("data/Gitter/Gitter_50km.shp") #Einlesen der shapefile

# Country boundaries
pakistan <- getData("GADM", country="Pakistan", level=0)
pakistan <- spTransform(pakistan, crs(raster_50))




# Distribution data  -------------------------------------------------------------------------------------------------
all_files_in_distribution     <- list.files(path = "../data/distribution/Pakistan/", recursive = T)

shp_paths                     <-  grep(".shp$", all_files_in_distribution, value=TRUE)

# shp_paths                     <-  shp_paths[260:429]

shp_list <- list() 
for(i in 1:length(shp_paths)){
     shp_list[[i]]                 <- readOGR(paste0("../data/distribution/Pakistan/", shp_paths[i]))
     shp_list[[i]]@data$species    <- gsub(".shp", "", basename(shp_paths[i]))
}


# Check projections
# lapply(shp_list, FUN=crs)

shp_merged <- do.call(raster::bind, shp_list, quote=T)

# Adjust column names
shp_merged@data$subspecies <- as.character(shp_merged@data$subspecies)
shp_merged@data$Subspecies <- as.character(shp_merged@data$Subspecies)

shp_merged@data$subspecies[!is.na(shp_merged@data$Subspecies)] <- shp_merged@data$Subspecies[!is.na(shp_merged@data$Subspecies)]
shp_merged@data$Subspecies <- NULL

shp_merged@data$id <- 0
shp_merged@data$Id <- NULL


# table(shp_merged@data$subspecies, useNA="always")
# table(shp_merged@data$Subspecies, useNA="always")

str(shp_merged@data)


writeOGR(shp_merged, "output/distribution_merged_PK/", driver="ESRI Shapefile", layer="distribution_Pakistan_all_test", overwrite_layer=TRUE)



## testplot ----------------------------------------------------------------------------------------------------------------------

shp_merged_rep <- spTransform(shp_merged, crs(raster_50))

plot(pakistan)
plot(raster_50, add=TRUE)
plot(shp_merged_rep, add=TRUE)




### Rest -------------------------------------------------------------------------------------------





# Some cities and villages  ------------------------------------------------------------------------------------------------------------------
cities <- c("Islamabad", "Gilgit", "Chitral", "Lahore", "Karachi", "Ghazi", "Quetta", "Peshawar")  


# conveninence function to look up and format results  
GNsearchAF <- function(x) {  
     res <- GNsearch(name=x, country="PAK", username="dirk.zeuss")  
     return(res[1, ])  
}

options(geonamesUsername="dirk.zeuss")
options(geonamesHost="api.geonames.org")

source(system.file("tests","testing.R",package="geonames"),echo=TRUE)


GNsearch(name="Islamabad")


# loop over city names and reformat  
GNresult <- sapply(cities, GNsearchAF)  
GNresult <- do.call("rbind", GNresult)  
GNresult <- cbind(city=row.names(GNresult), subset(GNresult, select=c("lng", "lat", "adminName1"))) 

