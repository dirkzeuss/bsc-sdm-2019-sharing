##Quelle: Dr. Bharatendra Rai - https://www.youtube.com/watch?v=pS5gXENd3a4

#Installation der Packages & hinzufuegen der Daten
install.packages("datasets")
library(datasets)
library(e1071)
library(ggplot2)

data(iris) #drei verschiedene Pflanzenarten
str(iris) 

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, colour = Species)) +
  geom_point() +
  labs(title = 'Sepal Length vs Sepal Width')
#Sepal = Kelchblatt

ggplot(iris, aes(x = Petal.Length, y = Petal.Width, colour = Species)) +
  geom_point() +
  labs(title = 'Petal Length vs Petal Width')
#Petal = Bluetenblatt

#------------

#Generierung des Models
mymodel <- svm(Species~., data=iris) 
summary(mymodel)

#Plot der SVM Classification
plot(mymodel, data=iris, 
     Petal.Width~Petal.Length,
     slice = list(Sepal.Width = 3, Sepal.Length = 4))

#-----------

#Auswertung des Modells
pred <- predict(mymodel, iris) 

tab <- table(Predicted = pred, Actual = iris$Species)
tab

1-sum(diag(tab))/sum(tab) #Gibt in Prozent an, wie genau das Modell ist
