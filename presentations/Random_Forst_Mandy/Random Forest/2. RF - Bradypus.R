#### Packages #### 
library(maptools) #includes sp
library(dismo) #includes raster
library(randomForest)
#library(raster) #already activated through dismo
#library(sp) #already acktivated through maptools


#### Environmental data ####

# loading Predictor Files, used to predict a feature (Merkmal) variable
p_files <- list.files(path=paste(system.file(package="dismo"),'/ex', sep=''), pattern='grd', full.names=TRUE )
#p_files
predictors <- stack(p_files) #Raster-stacking 9 predictor files to one file
predictors # show Raster-Stack information
names(predictors) # show names of predictors
plot(predictors)

#library(maptools)
data(wrld_simpl) # load worldmap
# loading Data of Bradypus
file <- paste(system.file(package="dismo"), "/ex/bradypus.csv", sep="")
bradypus <- read.table(file, header=TRUE, sep=',')

# first row is not necessary (Tierart), just lon and lat
bradypus <- bradypus[,-1]

# To speed up processing, restrict the predictions to a more restricted area (defined by a rectangular extent)
ext <- extent(-90, -32, -33, 23)

# Plotting, first layer of the RasterStack (Bio1)
plot(predictors, 1, ext=ext) #plot Bio1
plot(wrld_simpl, add=TRUE) #plot worldmap on top of Bio1, note the "add=TRUE" argument with plot
points(bradypus, col='blue') #add point on top of predictors and worldmap







#### Data Preparaion ####

# We now have a set of predictor variables (rasters) and presence-points.
# Next Steps: extracting every value of every predictor (except 'biome') at the locations of the presence-points.
# Why? To know which climatical conditions the bradypus likes to generate a trainset for RF
# 1 all presence-points
# 2 500 random background points (absence-points) 
# 3 combine these into two categories a single data.frame in which the first column (variable 'pb') indicates 
#    whether this is a presence (p) or a background (b) point.

#1 all presence-points
presence <- extract(predictors, bradypus) # Extracting presence values from rasters
tail(presence) # show last 6 entries

#2 setting random seed to always create the same random set of points for this example
set.seed(0) #why zero?
background <- randomPoints(predictors, 500) # creating 500 random absence-points
absence <- extract(predictors, background) # Extracting absence values from rasters

#3 combining presence and absence with value 1 (present) and value 0 (absent)
pb <- c(rep(1, nrow(presence)), rep(0, nrow(absence))) 
sdmdata <- data.frame(cbind(pb, rbind(presence, absence))) # putting pb and climate data into a single dataframe 
sdmdata
tail(sdmdata)

# biome is categorical variable (called a 'factor' in R ) and it is important to explicitly define it that way, 
# so that it won't be treated like any other numerical variable.
sdmdata[,'biome'] = as.factor(sdmdata[,'biome']) # set column 'biome' as factor, otherwise Regression
sdmdata[,'pb'] = as.factor(sdmdata[,'pb']) # set column 'pb' as factor, otherwise Regression
head(sdmdata) #show head of table, pb value 1 = presence
tail(sdmdata) #show tail of table, pb value 0 = absence
summary(sdmdata)






#### Training and Testing ####
#see Skript "Training and Testing" ?!








#### Random Forest ####
# https://www.stat.berkeley.edu/~breiman/RandomForests/cc_home.htm#intro
#library(randomForest)

# The Random Forest method is an extension of Classication and regression trees (CART).
# Random Forest grows many classification trees. To classify a new object from an input vector, 
# put the input vector down each of the trees in the forest. Each tree gives a classification, and we say the tree "votes" for that class. 
# The forest chooses the classification having the most votes (over all the trees in the forest).

# The function randomForest can take a formula or, in two separate arguments, a data.frame with the predictor variables, 
# and a vector with the response. If the response variable is a factor (categorical), randomForest will do classification, 
# otherwise it will do regression. 
# Whereas with species distribution modeling we are often interested in classification (species is present or not), 

#randomForest()	       - Methode
#var_ergebnis ~.,	     - Variable var_ergebnis wird vorausgesagt: Presence oder Absence; Tilde mit Punkt = Vorhersage auf allen verbliebenden Attributen au?er var_ergebnis (sonst spezielle Angabe der Attribute)
#data=datasetTrain,	   - Datensatz f?r Bootstrap-Stichprobe
#importance=TRUE, 	   - Beachtung der Variablenwichtigkeit
#mtry=3, 	             - Anzahl der Variablen f?r jeden Split
#ntree=100, 	         - wie viele B?ume der Wald umfassen soll; je mehr, desto besser, aber umso l?nger dauert die Berechnung
#replace=TRUE, 	       - gezogene Elemente werden zur?ck oder nicht zur?ck gelegt (nicht im Sinne des Algorithmus)
#do.trace=TRUE, 	     - Fortschrittsbalken beim Erzeugen des Waldes
#keep.forest=TRUE	     - Wald wird nicht verworfen, sondern f?r Klassifikationen gespeichert (und zum B?ume betrachten)


set.seed(123) #sorgt daf?r, dass bei zwei unterschiedlichen Durchl?ufen, dieselben Ergebnisse auftauchen (anderer Seed, anderes Ergebnis)

myForest<- randomForest(pb ~., data=sdmdata, importance=TRUE, mtry=3, ntree=100000, replace=TRUE, do.trace=TRUE, keep.forest=TRUE)
myForest


#Baum Nr. 47 anschauen anschauen; Repr?sentation als Matrix
getTree(myForest, k=47, labelVar=TRUE) #wald ?bergeben myRF; k = Baumnummer 47; labelvar = Namen der Variablen, sonst Nummern
#Spalte 1 = Wurzel von Baum k fragt, ob split var < split point ist; bedingung erf?llt, dann immer left daughter, bedingung nicht erf?llt right daughter
##Status 1 = innerer Knoten/Wurzel; -1 = Blatt (Ende)-> Vorhersage des Ergebnisses -> 0 oder 1

#### OOB-Daten ####
#gute Sch?tzung f?r Generalisierungsfehler

#Konfusionsmatrix mit den OOB-Daten
myForest$oob.times
#Stichprben-Nr. y wurde xx Mal nicht zur Erstellung von B?umen verwendet, .... , xx B?ume werden zu einem Teilwald verschmolzen -> erneute Vorhersage zur Absch?tzung, wie gut der Wald mit neuen Daten zurecht kommt

mean(myForest$oob.times)
#jede Stichprobe wurde zu xx Mal der B?ume nicht verwendet


#OOB-Fehlersch?tzung auslesen
myForest$err.rate
#Matrix mit n Beobachtungen - Zeile n = oob aus myForest, Prozensatz f?rs Vertun bei Presence | Absence

plot(myForest$err.rate[,1], type="l")
# x Zeile = B?ume, y = OOB-Fehler
# Entwicklung des OOB ?ber zunehmende Baumanzahl
# Grund: Wie viele B?ume soll ich erstellen innerhalb eines Waldes? Gro?e Schwankungen bei niedriger Baumanzahl, Einpendeln bei h?herer Baumzahl



#### Vorhersage ####
# 1. Beobachtungen durch den Wald schleusen
# Berechnen des Wiedereinsetzungsfehlers
klasseTrain <- sdmdata$pb #Beobachtungen auslesen und in variable speichern
#klasseTrain

vorhersage1 <- predict(myForest, newdata=sdmdata) #predict-Befehl zur Vorhersage, mit den Beobachtungen
vorhersage1 # Ergebnis der Vorhersage
tab <- table(klasseTrain,vorhersage1) #speichern der Matrix in Tabelle
tab # perfektes Ergebnis - perfekte Vorhersage ?

# 2. neue Beobachtungen - Vorhersage
# Berechnung des Generalisierungsfehlers 
# nicht berechenbar, weil kein zweiter Datensatz vorhanden; m?glich bradypus datensatz am Anfang zu teilen und so zwei datens?tze zum vergleichen zu erstellen





# Wie Vorhersage machen? Trainingsdata does not match the testdata (pb fehlt)






# Accuracy; Anteil aller Objekte, f?r die eine korrekte Vorhersage get?tigt wurde; Zugriff auf Presence/Presence + Absence/Absence / Anzahl der Beobachtungen (100)
 #(tab[1,1]+tab[2,2])/length(KlasseTest) # in xx% der F?lle korrekte Vorhersage


# Variablenwichtigkeit
imp <- varImpPlot(myForest) #variable importance plot - Ranking der Variablenwichtigkeit in zwei Verfahren; oben wichtig, unten unwichtig; "mittlere verlorene Votes"
imp

# Wie oft wurde welche Variable verwendet
varUsed(myForest, by.tree=FALSE, count=TRUE)
head(sdmdata) #zeige variablen noch mal

# Vorhersage plotten
r = raster(predictors, 1)
par(mfrow=c(1,2))
plot(!is.na(r), col=c('white', 'brown'), ext=ext ,legend=FALSE, main='Presence of Bradypus')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(bradypus, col='black') #add point on top of predictors and worldmap

pr <- raster::predict(predictors, myForest, ext=ext)
#par(mfrow=c(1,2))
plot(pr, col=c('brown', 'black'), legend=FALSE, main='Random Forest, Classification')
plot(wrld_simpl, add=TRUE, border='dark grey')

pdf("test2.pdf")
plot(pr, col=c('brown', 'black'), legend=FALSE, main='Random Forest, Classification')
points(bradypus, col='black') #add point on top of predictors and worldmap
dev.off()

#R?ckschluss auf Training and Testing
#erf <- evaluate(testpres, testbackg, myForest2) #evaluating rf1
#erf

#tr <- threshold(erf, 'spec_sens')
#plot(pr > tr, main='presence/absence')
#plot(wrld_simpl, add=TRUE, border='dark grey')
#points(pres_train, pch='+')
#points(backg_train, pch='-', cex=0.25)



#### Info Dataset ####

# PREDICTORS
# BIO1 = Annual Mean Temperature
# BIO5 = Max Temperature of Warmest Month
# BIO6 = Min Temperature of Coldest Month
# BIO7 = Temperature Annual Range (BIO5-BIO6)
# BIO8 = Mean Temperature of Wettest Quarter
# BIO12 = Annual Precipitation
# BIO16 = Precipitation of Wettest Quarter
# BIO17 = Precipitation of Driest Quarter
# BIOME = 14 Biome

# Biome: WWF TERRESTRIAL ECOREGIONS
# 01 Deserts and xeric shrublands
# 02 Tropical and subtropical moist broadleaf forests
# 03 Tropical and subtropical dry broadleaf forests
# 04 Tropical and subtropical coniferous forests
# 05 Temperate broadleaf and mixed forests
# 06 Temperate Coniferous Forest
# 07 Boreal forests / Taiga
# 08 Tropical and subtropical grasslands, savannas and shrublands
# 09 Temperate grasslands, savannas and shrublands
# 10 Flooded grasslands and savannas
# 11 Montane grasslands and shrublands
# 12 Tundra
# 13 Mediterranean Forests, woodlands and scrubs
# 14 Mangroves

# This Predictors scheme follows that of ANUCLIM, except that for 
# temperature seasonality the standard deviation was used 
# because a coefficient of variation does not make sense with 
# temperatures between -1 and 1). To create these values yourself, 
# you can use the 'biovars' function in the R package dismo
