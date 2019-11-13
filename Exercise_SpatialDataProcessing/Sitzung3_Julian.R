chooseCRANmirror(30)
options("repos")
local({r <- getOption("repos")
r["CRAN"] <- "https://ftp.gwdg.de/pub/misc/cran/"
options(repos=r)
})

#install.packages("sp")
library(sp)
#install.packages("raster")
library(raster)
#install.packages("rgdal")
library(rgdal)

#install.packages("maps")
#install.packages("maptools")
library(maps)
library(maptools)

prec <- getData("worldclim", var="prec", res=10, path="../data")
prec

plot(prec$prec1) #Ausw?hlen, welcher Layer geplottet werden soll mit $

#Frankreich mit Grenze
fra <- getData('GADM', country='FRA', level=0)
fra  

plot(fra)
plot(prec$prec1)
plot(fra, add=T)

#Ausstanzen auf einen einzelnen prec(vorherige Rasterdatei)
cropped_prec <- crop(prec, extent(fra))
plot(cropped_prec$prec1)

#Ausstanzen des Vektorlayers auf den Rasterlayer
clipped_prec <- mask(cropped_prec, fra) #2tes der Layer der ausgestantzt wird
plot(clipped_prec$prec1)

#Summieren der Werte(z.B. Niederschlag)
clipped_prec_sum <- sum(clipped_prec)
clipped_prec_sum_2 <- raster::stackApply(clipped_prec, rep(1,12), sum, na.rm=FALSE)
plot(clipped_prec_sum)

#Erstellen einer Karte mit den Umriss von Frankreich + einen Punkt Paris
plot(clipped_prec_sum)
plot(fra, add=T)
points(2.349014, 48.864716, pch=8, cex=2) # roughly the location of Paris

#SPeichern der Datei im Homeverzeichnis
jpeg("FirstSimpleMap.jpg")
plot(clipped_prec_sum)
plot(fra, add=T)
points(2.349014, 48.864716, pch=8, cex=2)
dev.off()

#Aufgaben
prec <- getData("worldclim", var="prec", res=10)
de <- getData('GADM', country='DE', level=0)

plot(DE_alt)
plot(de, add=T) #Plottet Grenze

DE_alt <- getData("alt",country="DE", res=10)
plot(DE_alt)
DE_alt


x <- c(7.4652981, 9.7019675, 7.8502943, 9.5722309, 9.4797461, 8.796667, 13.413215, 8.6821267, 11.4719457)
y <- c(51.5135872, 50.8701345, 51.0282107, 49.9892207, 51.3127114, 51.058889, 52.521918, 50.1109221, 49.4937364)
name <- c("Dortmund", "Bad_Hersfeld", "Olpe", "Lohr", "Kassel", "Frankenberg", "Berlin", "Frankfurt", "Happburg")
attributes <- as.data.frame(cbind(x,y))
xy <- cbind(x,y)
SPDF <- SpatialPointsDataFrame(xy, attributes)
proj4string(SPDF) <- crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")
SPDF_1 <- spTransform(SPDF, CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))

natural <- readOGR("natural.shp")

levels(natural@data$type)
river <- river[river$type == "riverbank",]

plot(DE_alt, main="Largest rivers in Germany")
plot(SPDF_1, pch = 19, add=T)

plot(river, lwd=1, add=T)

M?nchen <- points(11.581981, 48.135125, pch=22, col="red")
Hamburg <- points(9.993682, 53.551085, pch=22, col="red")
Berlin <- points(13.413215, 52.521918, pch=22, col="red")
Marburg <- points(8.7667933, 50.8021728, pch=21, col="red")

legend("bottomright",legend=c("river", "cities", "three largest cities", "Marburg"),
        cex=0.4, col = c("black", "black", "red","red"), lty = c(1,NA,NA, NA), pch = c(NA, 19, 22, 21))

text(15, 50, "Bad Hersfeld - Marburg = 66km, Frankfurt - Marburg = 75km,
Berlin - Marburg = 372km,Dortmund - Marburg = 120km, 
Lohr - Marburg = 107km,Olpe - Marburg = 70km,
Kassel - Marburg = 76km, Frankenberg - Marburg = 28km,
Happburg - Marburg = 241km ", cex=0.17)

