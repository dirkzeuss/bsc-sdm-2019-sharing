##Quelle: Babak Naimi - http://www.biogeoinformatics.org/ & https://www.youtube.com/watch?v=wLGanYOLzV8&t=1591s

install.packages("dismo")
library(dismo)

##Download und Bearbeitung der Daten einer (Tier)Art
lynx <- gbif("Lynx", "pardinus", download=T, geo=T, sp=F)
# " " = Name der Art, geo = TRUE -> nur Daten mit Georeferenzierung, 
# sp = FALSE -> Daten werden in data.frame konvertiert

class(lynx) #Zeigt Objekttyp an

w <- which(is.na(lynx$lon)) #ueberprueft die lon-Spalte, ob NA beinhaltet
w #integer(0) = beinhaltet kein NA mehr
lynx <- lynx[-w,] #Zeilen mit NA werden gelöscht
w <- which(is.na(lynx$lat)) #ueberprueft die lat-Spalte, ob NA beinhaltet

lynx$species <- 1 #fügt eine neue Spalte mit species hinzu
sp <- sp[,c('lon', 'lat', 'species')] #Beschriftung der Spalten
head(lynx)

coordinates(lynx) <- ~lon + lat #erstellt einen SpatialPointsDataFrame
class(lynx)

#---------------

#Download Predictor Daten

bio <- raster::getData('worldclim', var='bio', res=10)
bio

install.packages("usdm")
library(usdm)

v1 <- vifstep(bio) #Ueberprueft Predictor Daten nach ihrer Korrelation
v1

biom <- exclude(bio, v1) #Entfernt Predictor Daten mit einer geringen Korrelation
biom

plot(biom[[1]]) #plottet biom1
points(sp, cex=0.5, pch=16) #fuegt Lynx pardinus Daten hinzu

proj4string(lynx) <- projection(raster()) #projiziert das sp Objekt in das Koordinatensystem WGS84

#---------------

#Modelle erstellen - Support Vector Machine
install.packages("sdm") #sdm = species distribution model
library(sdm)

d <- sdmData(species ~., lynx, predictors = biom, bg = list(n=1000))
d
# lynx = Art, predictors umweltdaten, bg = fuegt background Daten hinzu

getmethodNames() #Anzeige der verfuegbaren Methoden

m <- sdm(species ~., d, methods=c('svm')) #Anwendung der Methode
m

p <- predict(m, biom) # erstellt die Karte anhand des Modells und der Umweltdaten

plot(p) #Plottet Karte mit Verbreitungsgebiet


