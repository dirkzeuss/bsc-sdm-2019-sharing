library("raster")
install.packages("rgdal")
library("rgdal")
library("maptools")

all_files_in_distribution <- list.files(path = "F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/Ladak_maps_final" , recursive = T)

shp_paths                     <-  grep(".shp$",all_files_in_distribution , value=TRUE)

shp_list <- list() 
for(i in 1:length(shp_paths)){
  shp_list[[i]]                 <- readOGR((paste0("F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/Ladak_maps_final/", shp_paths[i])))
  shp_list[[i]]@data$species    <- gsub(".shp", "", basename(shp_paths[i]))
}

shp_merged <- do.call(raster::bind, shp_list, quote=T)


shp_merged@data$subspecies[!is.na(shp_merged@data$Subspecies)] <- shp_merged@data$Subspecies[!is.na(shp_merged@data$Subspecies)]
shp_merged@data$Subspecies <- NULL

shp_merged@data$id <- 0
shp_merged@data$Id <- NULL
shp_merged

writeOGR(shp_merged, "F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/Ladak_maps_final", driver="ESRI Shapefile", layer="distribution_Ladakh_all_test", overwrite_layer=TRUE)

pak <- readOGR("F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/distribution_merged_Pakistan/distribution_Pakistan_all_test.shp")
lad <- readOGR("F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/Ladak_maps_final/distribution_Ladakh_all_test.shp")

pak_df <- as.data.frame(pak)
lad_df <- as.data.frame(lad)

all <- merge(pak_df,lad_df, all.x = TRUE, all.y = FALSE)

all$id <- NULL

write.table(all, file = "F:/Projekt_Parkistan/bsc-sdm-2010-hessenbox/PakistanLadak.csv", quote = FALSE, row.names = FALSE, sep = ";")

