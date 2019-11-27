library(dismo)
library(maptools)
data("wrld_simpl")
predictors <- stack(list.files(file.path(system.file(package="dismo"), 'ex'), pattern = 'grd$', full.names = TRUE))
file <- file.path(system.file(package = "dismo"), "ex/bradypus.csv")
bradypus <- read.table(file, header = TRUE, sep=',')
bradypus <- bradypus[,-1]
presvals <- extract(predictors, bradypus)
set.seed(0)
backgr <- randomPoints(predictors, 500)
absvals <- extract(predictors, backgr)
pb <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
sdmata <- data.frame(cbind(pb, rbind(presvals, absvals)))
sdmata[,'biome'] <- as.factor(sdmata[,'biome'])


pred_nf <- dropLayer(predictors, 'biome')


set.seed(0)
group <- kfold(bradypus, 5)
pres_train <- bradypus[group !=1, ]
pres_test <- bradypus [group ==1, ]

ext <- extent(-90, -32, -33, 23)

set.seed(10)
backg <- randomPoints(pred_nf, n=100, ext=ext, extf = 1.25)
colnames(backg) = c('lon', 'lat')
group <- kfold(backg, 5)
backg_train <- backg[group == 1, ]
backg_test <- backg[group == 1, ]


r <- raster(pred_nf, 1)
plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
plot(ext, add=TRUE, col='red', lwd=2)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')




bc <- bioclim(pred_nf, pres_train)
plot(bc, a=1, b=2, p=0.85)

e <- evaluate(pres_test, backg_test, bc, pred_nf)
e


tr <- threshold(e, 'spec_sens')
tr

pb <- predict(pred_nf, bc, ext=ext, progress='')
pb

par(mfrow=c(1,2))
plot(pb, main='Bioclim, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
plot(pb > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

dm <- domain(pred_nf, pres_train)
e <- evaluate(pres_test, backg_test, dm, pred_nf)

pd = predict(pred_nf, dm, ext=ext, progress='')
par(mfrow=c(1,2))
plot(pd, main='Domain, raw values')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(pd > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

mm <- mahal(pred_nf, pres_train)
e <- evaluate(pres_test, backg_test, mm, pred_nf)
e


pm = predict(pred_nf, mm, ext=ext, progress='')
par(mfrow=c(1,2))
pm[pm < -10] <- -10
plot(pm, main='Mahalanobis distance')
plot(wrld_simpl, add=TRUE, border='dark grey')
tr <- threshold(e, 'spec_sens')
plot(pm > tr, main='presence/absence')
plot(wrld_simpl, add=TRUE, border='dark grey')
points(pres_train, pch='+')

