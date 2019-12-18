#### Training and Testing ####

# Most modelers rely on cross-validation. This consists of creating a model
# with one 'training' data set, and testing it with another data set of known occurrences.
# Typically, training and testing data are created through random sampling (without replacement) from a single data set.


# First we make a training and a testing set of Bradypus Data
group <- kfold(bradypus, 5) #creating a group of 5 with data randomly asssigned to one group 
pres_train <- bradypus[group != 1, ] #training set presence is 'not 1'
pres_test <- bradypus[group == 1, ] #testing set presence 'is 1'

# Second we make a training and a testing set of Background Data

# The first layer in the Raster-Stack is used as a 'mask'. 
# That ensures that random points only occur within the spatial extent 
# of the rasters, and within cells that are not NA, and that there is only 
# a single absence point per cell. Here we further restrict the background points 
# to be within 12.5% of our specified extent 'ext'.

backg <- randomPoints(predictors, n=1000, ext=ext, extf = 1.25) 
# Mask = predictors, n = number of points, ext = extend, extf =increases extent), why extf??
#colnames(backg) = c('lon', 'lat') # renaming colnames from x to lon and y to lat
group <- kfold(backg, 5) # creating a group of 5 with data randomly asssigned to one group
backg_train <- backg[group != 1, ] # creating training set presence is not 1
backg_test <- backg[group == 1, ] # creating testing set presence is 1

# Show Training and Testing Points
r = raster(predictors, 1)
plot(r)

plot(!is.na(r), col=c('light blue', 'brown'), legend=FALSE) # making the plot two-coloured for better vision
plot(ext, add=TRUE, col='white', lwd=2) # show extent
points(pres_train, pch= '+', cex=1.0, col='white') #Points trainingspoints present
points(pres_test, pch='+', cex=1.0, col='black')#Points testpoints present

plot(!is.na(r), col=c('light blue', 'brown'), legend=FALSE) # making the plot two-coloured for better vision
plot(ext, add=TRUE, col='white', lwd=2) # show extent
points(backg_test, pch='-', cex=0.5, col='white') #show background testpoints absent
points(backg_train, pch='-', cex=0.5, col='black') #show background trainingspoints absent


# we cannot fit the model with a Raster-Stack and points. 
# Instead, we need to extract the environmental data values ourselves, 
# and fit the models with these values.

train <- rbind(pres_train, backg_train) # combine presence and background training-data
pb_train <- c(rep(1, nrow(pres_train)), rep(0, nrow(backg_train))) # replicates the values??
envtrain <- extract(predictors, train) # extracting environmental training data from predictors
envtrain <- data.frame( cbind(pa=pb_train, envtrain) ) # binding them into a dataframe
envtrain[,'biome'] = factor(envtrain[,'biome'], levels=1:14) #??
head(envtrain)
tail(envtrain)

testpres <- data.frame( extract(predictors, pres_test) ) # ???
testbackg <- data.frame( extract(predictors, backg_test) ) #???

testpres[ ,'biome'] = factor(testpres[ ,'biome'], levels=1:14) # ???
testbackg[ ,'biome'] = factor(testbackg[ ,'biome'], levels=1:14) #???

#### Random Forest ####