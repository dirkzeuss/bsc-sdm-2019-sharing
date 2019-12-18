# Hintergrund: 100 M?nner haben 100 Frauen in einer Bar angesprochen mit der Ausicht auf "Erfolg oder Korb".
# verwendete Variablen/Attribute
# Altersunterschied zu der Frau, die er ansprechen will
# IQ
# konsumierte Biergl?ser
# Aufenthaltsdauer in Minuten
# Haarfarbe
# hat Freundin true/false

#### Datens?tze ####
#Datens?tze laden und betrachten

#Datensatz zum Trainieren des Waldes
datasetTrain <- read.table("exampleDatasetTrain.csv", sep = ",", header=TRUE)
#zum Testen des Waldes mit neuen Daten
datasetTest <- read.table("exampleDatasetTest.csv", sep = ",", header=TRUE)

#zeige Datenstruktur
#str(datasetTrain)
#str(datasetTest)

#Zeige erste 6 Zeilen des Datensatzes
head(datasetTrain)


#### Random Forest ####
#install.packages("randomForest")
library("randomForest")

#Wald generieren
set.seed(123) #sorgt daf?r, dass bei zwei unterschiedlichen Durchl?ufen, dieselben Ergebnisse auftauchen (anderer Seed, anderes Ergebnis)

myRF <- randomForest(ergebnis ~ ., data=datasetTrain, importance=TRUE, mtry=2,
                     ntree=100, replace=TRUE, do.trace=TRUE, keep.forest=TRUE)

#randomForest()	       - Methode
#ergebnis ~.,	         - Variable Ergebnis wird vorausgesagt: Erfolg oder Korb; Tilde mit Punkt = Vorhersage auf allen verbliebenden Attributen au?er Ergebnis (sonst spezielle Angabe der Attribute)
#data=datasetTrain,	   - Datensatz f?r Stichprobe 
#importance=TRUE, 	   - Beachtung der Variablenwichtigkeit
#mtry=3, 	             - Anzahl der Variablen f?r jeden Split
#ntree=100, 	         - wie viele B?ume der Wald umfassen soll; je mehr, desto besser, aber umso l?nger dauert die Berechnung
#replace=TRUE, 	       - Objekte werden zur?ck oder nicht zur?ck gelegt (nicht im Sinne des Algorithmus)
#do.trace=TRUE, 	     - Fortschrittsbalken beim Erzeugen des Waldes
#keep.forest=TRUE	     - Wald wird nicht verworfen sondern f?r Klassifikationen gespeichert und zum B?ume anschauen


myRF
#oob error rate: 18% - bei 100 neuen Daten werden 18 falsch zugeordnet

#summary(myRF)

#Baum Nr. 47 anschauen anschauen; Repr?sentation als Matrix
getTree(myRF, k=47, labelVar=TRUE) #wald ?bergeben myRF; k = Baumnummer 47; labelvar = Namen der Variablen, sonst Nummern

#Spalte 1 = Wurzel von Baum k fragt, ob split var (aufhenthalsdauer) kleiner split point (42.5...) ist; bedingung erf?llt, dann immer left daughter, bedingung nicht erf?llt right daughter
#Status 1 = innerer Knoten/Wurzel; -1 = Blatt -> Vorhersage Ergebnis

#Wald erweitern, wenn B?ume nicht reichen
#myRF2 <- grow(myRF, how.many=50)
#myRF2 # 100 aus myRF + 50 = 150

#Mehrere B?ume zu Wald kombinieren
#myRF3 <- combine(myRF, myRF2)
#myRF3 # 100 aus myRF + 150 aus myRF2 = 250



#### OOB-Daten ####
#gute Sch?tzung f?r Generalisierungsfehler

#Konfusionsmatrix mit den OOB-Daten
myRF$oob.times
#Mann Nr. y wurde xx Mal nicht zur Erstellung von B?umen verwendet, .... , xx B?ume werden zu einem Teilwald verschmolzen -> erneute Vorhersage zur Absch?tzung, wie gut der Wald mit neuen Daten zurecht kommt

mean(myRF$oob.times)
#jeder Mann wurde zu xx Mal der B?ume nicht verwendet


#OOB-Fehlersch?tzung auslesen
myRF$err.rate
#Matrix mit 100 Beobachtungen - Zeile 100 = oob aus myRF, Prozensatz f?rs Vertun bei ?rfolg | Korb


plot(myRF$err.rate[,1], type="l")
# x Zeile = B?ume, y = OOB-Fehler
# Entwicklung des OOB ?ber zunehmende Baumanzahl
# Grund: Wie viele B?ume soll ich erstellen innerhalb eines Waldes? Gro?e Schwankungen bei niedriger Baumanzahl, Einpendeln bei h?herer Baumzahl



#### Vorhersage ####
#1. Beobachtungen durch den Wald schleusen
#Berechnen des Wiedereinsetzungsfehlers
wahleKlasseTrain <- datasetTrain$ergebnis #Beobachtungen auslesen und in variable speichern
wahleKlasseTrain

vorhersage1 <- predict(myRF, newdata=datasetTrain) #predict-Befehl zur Vorhersage, mit den Beobachtungen
vorhersage1 # ergebnis der Vorhersage
table(wahleKlasseTrain,vorhersage1) #perfektes Ergebnis - perfekte Vorhersage

#2. 100 neue Beobachtungen - Vorhersage
#Berechnung des Generalisierungsfehlers
wahleKlasseTest <- datasetTest$ergebnis
wahleKlasseTest
vorhersage2 <- predict(myRF, newdata=datasetTest)
vorhersage2

tab <- table(wahleKlasseTest,vorhersage2) #speichern der Matrix in Tabelle, Vorhersage nicht mehr perfekt
tab

#Accuracy; Anteil aller M?nner, f?r die eine korrekte Vorhersage get?tigt wurde; Zugriff auf Erfolg/Erfolg + Korb/Korb / Anzahl der Beobachtungen (100)
(tab[1,1]+tab[2,2])/length(wahleKlasseTest) # in xx% der F?lle korrekte Vorhersage


#Variablenwichtigkeit
imp <- varImpPlot(myRF) #variable importance plot - Ranking der Variablenwichtigkeit in zwei Verfahren; oben wichtig, unten unwichtig; "mittlere verlorene Votes"
imp


importance(myRF)
#Wie oft wurde welche Variable verwendet
varUsed(myRF, by.tree=FALSE, count=TRUE)
head(datasetTrain) #zeige variablen noch mal




