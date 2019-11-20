## What is "Boosted Regression Trees"? ####
#
# Following information extracted from: 
# "A working guide to boosted regression trees" by Elith et al. (2008)
#
# BRT: 
#
# - Ensemble of multiple methods to fit data to 
#   a statistical model.
# - combines the strength of two different algorithms: 
#   "Regression Trees" (predictors + response, recursive binary splits) 
#   and "Boosting" (adaptive method, combining several simple models for 
#   increased model performance)
# 
# benefits of BRT-modelling:
# 
# - handle different types of predictor variables
# - handles missing data automatically
# - can fit complex non-linear relationships, and automatically handles 
#   interaction effects between predictors
# - fitting multiple trees -> powerful ecological insight, superior 
#   predictive performance


## Boosted Regression Trees (BRT) - packages ####

install.packages("gbm")
install.packages("dismo") 

# "dismo": extension to "gbm" (easier applying to ecological data 
# and easier interpretation)

library(sp)
library(dismo)
library(raster)
library(gbm)


## load trainingdata ####

# Laden der Beispieldaten: Süsswasser-Aal (anguilla-eel)

data(Anguilla_train)
head(Anguilla_train)


## fit data of BRT-model ####


# is there enough data to model interactions of reasonable complexity? 
# -> 1000 sites, 202 presences of the eel
# which learning-rate to choose? which tree complexity?

angaus.tc5.lr01 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                            family = "bernoulli", tree.complexity = 5,
                            learning.rate = 0.01, bag.fraction = 0.5)

# data=Anguilla_train: used dataset
# gbm.x = c(3:13): predictor-variables
# gbm.y = 2: response-variable
# family = 'bernoulli': nature of error-structure
# bag.fraction = 0.5: Subsampling fraction - the fraction of the training set 
# observations randomly selected to propose the next tree in the expansion

## information about data and model .... ####

angaus.tc5.lr01[[29]] 
# pull out one component of the list

angaus.tc5.lr01$cv.statistics
# general statistics

length(angaus.tc5.lr01$fitted)
# size of the dataset, how many fitted values does it contain?

names(angaus.tc5.lr01)
# names of variables

summary(angaus.tc5.lr01)
# variable importance for prediction/response 

## choose settings ####

# last model output: only 650 trees: reduce learning rate-factor to produce a  
# number of over 1000 trees -> higher performance

angaus.tc5.lr005 <- gbm.step(data=Anguilla_train, gbm.x = 3:13, gbm.y = 2,
                             family = "bernoulli", tree.complexity = 5,
                             learning.rate = 0.005, bag.fraction = 0.5)


## simplify the model ####

angaus.simp <- gbm.simplify(angaus.tc5.lr005, n.drops = 5)

# n.drops = 5: number of dropped/dismissed variables (with low importance)
# gbm.x=angaus.simp$pred.list[[1]:  number indicated by the red vertical line,
# number of dropped/removed variables

angaus.tc5.lr005.simp <- gbm.step(Anguilla_train,
                                  gbm.x=angaus.simp$pred.list[[1]], gbm.y=2,
                                  tree.complexity=5, learning.rate=0.005)

# new, simplified model is produced, but we now continue with the not 
# simplified model...

## plotting functions and fitted values from the model ####

gbm.plot(angaus.tc5.lr005, n.plots=11, plot.layout=c(4, 3), write.title = FALSE)

# n.plots=11: number of plots
# plot.layout=c(4, 3): 3 rows and 4 columns layout
# write.title = FALSE: save space by not plotting the titles
# can give a misleading indication about the distribution of 
# the fitted values in relation to each predictor

gbm.plot.fits(angaus.tc5.lr005)

# plot the fitted values in relation to each 
# of the predictors used in the model

## interrogate and plot interactions ####

# pairwise interactions (find interactions and create hierarchy):

find.int <- gbm.interactions(angaus.tc5.lr005)
find.int$interactions
find.int$rank.list

# plot pairwise interactions:

gbm.perspec(angaus.tc5.lr005, 7, 1, y.range=c(15,20), z.range=c(0,0.6))

## predicting to new data ####

# set up a data.frame with rows for sites and columns for the 
# variables that are in your model
# predict to a set of sites (rather than to a whole map)

data(Anguilla_test)

preds <- predict.gbm(angaus.tc5.lr005, Anguilla_test,
                     n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")

# n.trees=angaus.tc5.lr005$gbm.call$best.trees: selects number of 
# trees with best performance

# calculate deviance:

calc.deviance(obs=Anguilla_test$Angaus_obs, pred=preds, calc.mean=TRUE)

# evaluation sites, evaluation with independant data:

d <- cbind(Anguilla_test$Angaus_obs, preds)
pres <- d[d[,1]==1, 2]
abs <- d[d[,1]==0, 2]
e <- evaluate(p=pres, a=abs)
e

# predict to a varying number of trees, predict to a vector of trees:

angaus.5000 <- gbm.fixed(data=Anguilla_train, gbm.x=3:13, gbm.y=2,
                         learning.rate=0.005, tree.complexity=5, n.trees=5000)

tree.list <- seq(100, 5000, by=100)
pred <- predict.gbm(angaus.5000, Anguilla_test, n.trees=tree.list, "response")

# Note that the code above makes a matrix, with each column being the 
# predictions from the model angaus.5000 to the number of trees specified 
# by that element of tree.list - 
# for example, the predictions in column 5 are for tree.list[5] = 500 trees

# calculate deviance of all results (using a loop) and plot them:

angaus.pred.deviance <- rep(0,50)
for (i in 1:50) {
  angaus.pred.deviance[i] <- calc.deviance(Anguilla_test$Angaus_obs,
                                           pred[,i], calc.mean=TRUE)
}
plot(tree.list, angaus.pred.deviance, ylim=c(0.7,1), xlim=c(-100,5000),
     type='l', xlab="number of trees", ylab="predictive deviance",
     cex.lab=1.5)


## spatial prediction ####

# predict to a whole map (technically to a RasterLayer object):

data(Anguilla_grids)
plot(Anguilla_grids)

# create data.frame and pass that on to the predict function:

Method <- factor('electric', levels = levels(Anguilla_train$Method))
add <- data.frame(Method)
p <- predict(Anguilla_grids, angaus.tc5.lr005, const=add,
             n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")
p <- mask(p, raster(Anguilla_grids, 1))

plot(p, main='Angaus - BRT prediction')