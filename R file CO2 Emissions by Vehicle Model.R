#set wd

Emsns <- read.csv("Gas_Hybrid_3.csv", header=T)
head(Emsns)
summary(Emsns)
attach(Emsns)
names(Emsns)
nrow(Emsns)
ncol(Emsns)
typeof(CO2Emissions)
typeof(Clndrs)
typeof(engineSize)
typeof(EngineSize)
typeof(fuelConsumption)
typeof(FuelConsumption)
str(Emsns)

######VISUALIZATIONS#####

#exploring vehicle traits before assigning variables as categorical:
pairs(~ CO2Emissions + EngineSize + Clndrs + FuelConsumption, 
      data = Emsns, panel = panel.smooth)

plot(EngineSize,CO2Rating)
plot(EngineSize,SmogRating)

plot(Clndrs,CO2Rating)
plot(Clndrs,SmogRating)


#the following three categories have been eliminated, with the exception
#of ComboFuelConsumption, which was kept and renamed "FuelConsumption".
#To see them again, use "Gas and Hybrid Combined" sheet in the Excel file
#"Canada_2020 car models_Compilation", which has the original 920 records 
#as opposed to 915 in the cleansed data set
#plot(CityFuelConsumption,CO2Emissions)
#plot(HwyFuelConsumption,CO2Emissions)
#plot(ComboFuelConsumption,CO2Emissions)

plot(CO2Emissions,CO2Rating)
plot(CO2Emissions,SmogRating)
plot(CO2Rating,SmogRating)

#medians: CO2Emissions,CO2Rating,SmogRating
median(CO2Emissions) #248
median(CO2Rating) #4
median(SmogRating) #5

plot(FuelConsumption,SmogRating)
plot(FuelConsumption,CO2Rating)


#assigning variables as categorical and putting them back into 
#the Emsns data frame:
fac.VehicleType<- as.factor(VehicleType) 
VehicleType <- as.factor(VehicleType)
Emsns$VehicleType <- as.factor(VehicleType)
summary(Emsns)

fac.ParentCo <- as.factor(ParentCo) 
ParentCo <- as.factor(ParentCo)
Emsns$ParentCo <- as.factor(ParentCo)
summary(Emsns)

fac.CountryofOrigin <- as.factor(CountryofOrigin) 
CountryofOrigin <- as.factor(CountryofOrigin)
Emsns$CountryofOrigin <- as.factor(CountryofOrigin)
summary(Emsns)
Emsns

fac.HighSalesCountry <- as.factor(HighSalesCountry) 
HighSalesCountry <- as.factor(HighSalesCountry)
Emsns$HighSalesCountry <- as.factor(HighSalesCountry)
summary(Emsns)
Emsns

fac.Class <- as.factor(Class) 
Class <- as.factor(Class)
Emsns$Class <- as.factor(Class)
summary(Emsns)
Emsns

#may need to undo Cylinders as factor
fac.Clndrs <- as.factor(Clndrs) 
Clndrs <- as.factor(Clndrs)
Emsns$Clndrs <- as.factor(Clndrs)
summary(Emsns)
Emsns

fac.Transmission <- as.factor(Transmission) 
Transmission <- as.factor(Transmission)
Emsns$Transmission <- as.factor(Transmission)
summary(Emsns)
Emsns

fac.FuelType <- as.factor(FuelType) 
FuelType <- as.factor(FuelType)
Emsns$FuelType <- as.factor(FuelType)
summary(Emsns)
Emsns

fac.Make <- as.factor(Make) 
Make <- as.factor(Make)
Emsns$Make <- as.factor(Make)
summary(Emsns)
Emsns

plot(CO2Emissions, Class)
plot(CO2Emissions, Make)

library(ggplot2)
ggplot(data=Emsns, aes(x=Transmission, y=CO2Emissions)) + geom_bar(stat="identity", width=0.5)

#country by car data
ggplot(data=Emsns, aes(x=CountryofOrigin, y=CO2Emissions)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=CountryofOrigin, y=FuelConsumption)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=HighSalesCountry, y=FuelConsumption)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=HighSalesCountry, y=CO2Emissions)) + geom_bar(stat="identity", width=0.5)

#company by car data
ggplot(data=Emsns, aes(x=ParentCo, y=CO2Emissions)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=ParentCo, y=FuelConsumption)) + geom_bar(stat="identity", width=0.5)

#Internet data visualizations
ggplot(data=Emsns, aes(x=HighSalesCountry, y=PercentageGlobalCO2EmissionsbyCountry)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=HighSalesCountry, y=PercentageGlobalCrudeOilImportsbyCountry)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=HighSalesCountry, y=LogisticsPercentageofGDPbyCountry)) + geom_bar(stat="identity", width=0.5)
ggplot(data=Emsns, aes(x=HighSalesCountry, y=NoVehiclesCntry)) + geom_bar(stat="identity", width=0.5)

#######REGRESSION########
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")
#install.packages("jsonlite", type = "source")
#library(MASS)

##MULTIPLE LINEAR REGRESSION
model1 <- lm(CO2Emissions ~ engineSize + Clndrs + fuelConsumption, data = Emsns)
summary(model1)
model1_summary <-summary(model1)
mean(model1_summary$residuals^2)
#CO2Emissions = 22.7510 + ((3.1423)EngineSize) + ((3.3626)Cylinders) + ((18.1984)FuelConsumption)

##BINARY LOGISTIC REGRESSION

#we will use engineSize + Clndrs + fuelConsumption predictors to predict the CO2Emissions label
EmsnsLevel <- ifelse(CO2Emissions <=248,"Low","High")
Emsns$EmsnsLevel <-EmsnsLevel
fac.EmsnsLevel <- as.factor(EmsnsLevel) 
EmsnsLevel  <- as.factor(EmsnsLevel)
Emsns$EmsnsLevel  <- as.factor(EmsnsLevel)
summary(Emsns)
Emsns

is.factor(EmsnsLevel)

glm.fits.Emsns <- glm(EmsnsLevel ~ engineSize + Clndrs + fuelConsumption, data = Emsns, family = binomial)

summary(glm.fits.Emsns)
model2_summary <- summary(glm.fits.Emsns)
coef(glm.fits.Emsns)
contrasts(Emsns$EmsnsLevel) 
glm.probs.Emsns <- predict(glm.fits.Emsns, type = "response")
glm.probs.Emsns[1:10] 
glm.pred.Emsns <- rep("high.pred", dim(Emsns)[1])
glm.pred.Emsns[glm.probs.Emsns > 0.8] = "low.pred"
table(glm.pred.Emsns, Emsns$EmsnsLevel)
(451 + 400) / dim(Emsns)[1] #rate of the diagonal term =
#success rate = 93.00546
(3 + 61) / dim(Emsns)[1] # training error rate or mis-classification rate =
#error rate = .06994536

###########

library(splines)

fuelConsumption.limits <- range(fuelConsumption) #range of all values of age from 
#smallest to largest
fuelConsumption.limits # 4 - 22
fuelConsumption.grid <- seq(fuelConsumption.limits[1], fuelConsumption.limits[2], 1)
fuelConsumption.grid

##Cubic Spline
splines.fit <- lm(CO2Emissions ~ bs(fuelConsumption, df = 6), data = Emsns)
pred <- predict(splines.fit, newdata = list(fuelConsumption = fuelConsumption.grid), 
                se = TRUE)
dim(bs(fuelConsumption, df = 6))
attr(bs(fuelConsumption, df = 6), "knots")

plot(Emsns$fuelConsumption, Emsns$CO2Emissions, xlim = fuelConsumption.limits, cex = 0.5, 
     col = "darkgrey")
lines(fuelConsumption.grid, pred$fit, lwd = 2, col = "blue")

se.bands <- cbind(pred$fit + 2*pred$se.fit, pred$fit - 2*pred$se.fit)
matlines(fuelConsumption.grid, se.bands, lwd = 1, col = "blue", lty = 3)

##Natural Cubic Spline
natural.splines.fit <- lm(CO2Emissions ~ ns(fuelConsumption, df = 4), data = Emsns)
pred2 <- predict(natural.splines.fit, 
                 newdata = list(fuelConsumption = fuelConsumption.grid), se = T)

dim(ns(Emsns$fuelConsumption, df = 4))
attr(ns(Emsns$CO2Emissions, df = 4), "knots")

plot(Emsns$fuelConsumption, Emsns$CO2Emissions, xlim = fuelConsumption.limits, 
     cex = 0.5, col = "darkgrey")
lines(fuelConsumption.grid, pred2$fit, lwd = 2, col = "red")
se.bands2 <- cbind(pred2$fit + 2*pred2$se.fit, pred2$fit - 
                     2*pred2$se.fit)
matlines(fuelConsumption.grid, se.bands2, lwd = 1, col = "red", lty = 3)

plot(Emsns$fuelConsumption, Emsns$CO2Emissions, xlim = fuelConsumption.limits, 
     cex = 0.5, col = "darkgrey")
lines(fuelConsumption.grid, pred$fit, lwd = 2, col = "blue")
matlines(fuelConsumption.grid, se.bands, lwd = 1, col = "blue", lty = 3)
lines(fuelConsumption.grid, pred2$fit, lwd = 2, col = "red")
matlines(fuelConsumption.grid, se.bands2, lwd = 1, col = "red", lty = 3)

##Local Regression
local.fit <- loess(CO2Emissions ~ fuelConsumption, span = 0.75, data = Emsns)

pred <- predict(local.fit, newdata = 
                  data.frame(fuelConsumption = fuelConsumption.grid))

plot(Emsns$fuelConsumption, Emsns$CO2Emissions, xlim = 
       range(Emsns$fuelConsumption), cex = 0.5, col = "darkgrey")
lines(fuelConsumption.grid, pred, col = "blue", lwd = 2)
legend("topright", legend = c("Span = 0.75"), 
       col = c("blue"), lty = 1, lwd = 2, cex = 0.8)

##KNN Regression
install.packages("FNN")
library(FNN)
dim(Emsns)[1] # to check number of rows (observations)
par(mfrow = c(1,1))
# Fit simple linear regression
plot(fuelConsumption, CO2Emissions)
slr.fit <- lm(CO2Emissions ~ fuelConsumption, data = Emsns)
summary(slr.fit)
# fitted values for first 10 observations
slr.fit$fitted[1:10] 
# fitted value for first observation
coef(slr.fit)[1] + coef(slr.fit)[2]*fuelConsumption[1] 

# Fit KNN regression with K = 5
knn.fit <- knn.reg(train = Emsns["fuelConsumption"], test = Emsns["fuelConsumption"], y = CO2Emissions, k = 5)
knn.fit$pred[1:10]

# find train MSE for simple linear regression
slr_train_MSE <- (1/dim(Emsns)[1])*sum((CO2Emissions-slr.fit$fitted)^2)
slr_train_MSE

# find train MSE for KNN regression with different K
knn_train_MSE <- rep(0, 50)
for(i in 1:50){
  knn.fit <- knn.reg(train = Emsns["fuelConsumption"], test = Emsns["fuelConsumption"], y = CO2Emissions, k = i)
  knn_train_MSE[i] <- (1/dim(Emsns)[1])*sum((CO2Emissions-knn.fit$pred)^2)
}

plot(1:50, knn_train_MSE, col="green", pch=20, xlab="K", ylab="Train MSE")
#abline(h=slr_train_MSE, lty=5)

# find test MSE 
# the validation set approach
# use first 180 observations as train set and remaining as validation set
slr.fit2 <- lm(CO2Emissions ~ fuelConsumption, data = Emsns[1:850,])
summary(slr.fit2)

pred <- predict(slr.fit2, data.frame(fuelConsumption = fuelConsumption[851:915])) 

# find test MSE for simple linear regression
slr_test_MSE <- (1/65)*sum((CO2Emissions[851:915]-pred)^2)
slr_test_MSE

# find test MSE for KNN regression for differnt K
knn_test_MSE <- rep(0, 50)
for(i in 1:50){
  knn.fit2 <- knn.reg(train = as.data.frame(fuelConsumption[1:850]), test = as.data.frame(fuelConsumption[851:915]), y = CO2Emissions[1:850], k = i)
  knn_test_MSE[i] <- (1/65)*sum((CO2Emissions[851:915]-knn.fit2$pred)^2)
}

plot(1:50, knn_test_MSE, col="green", pch=20, xlab="K", ylab="Test MSE")
abline(h=slr_test_MSE, lty=2)


#######DECISION TREE########

install.packages("tree")
library(tree)
set.seed(1156)

Emsns <- read.csv("Gas_Hybrid_4.csv", header=T)
head(Emsns)
summary(Emsns)
attach(Emsns)
names(Emsns)

train <- sample(1:nrow(Emsns), nrow(Emsns)/2)
tree.Emsns <- tree(CO2Emissions ~., subset = train, data = Emsns)
summary(tree.Emsns)#fuelConsumption and engineSize = variables used.
plot(tree.Emsns)
text(tree.Emsns, pretty = 0) 

yhat <- predict(tree.Emsns, newdata = Emsns[-train, ])
test.CO2Emissions <- Emsns[-train, 7] #7th column = CO2Emissions
cbind(yhat, test.CO2Emissions)[1:10,]
mean((yhat - test.CO2Emissions)^2)#MSE: 578.2233

#Pruning the Regression Tree to see if we can improve results:
cv.Emsns <- cv.tree(tree.Emsns) 
cv.Emsns 
which.min(cv.Emsns$dev) 
cv.Emsns$size[1] 

prune.Emsns <- prune.tree(tree.Emsns, best = 8) 
plot(prune.Emsns)
text(prune.Emsns, pretty = 0) #no change

#To see both original tree and pruned subtree:
par(mfrow = c(1,2))
plot(tree.Emsns)
text(tree.Emsns, pretty = 0)
plot(prune.Emsns)
text(prune.Emsns, pretty = 0)

#Use Pruned Tree to make predictions on the test set:
yhat <- predict(prune.Emsns, newdata = Emsns[-train, ])
test.CO2Emissions <- Emsns[-train, 7]
cbind(yhat, test.CO2Emissions)[1:10,] #prediction results compared to original data
mean((yhat - test.CO2Emissions)^2)#577.0371

## Bagging Trees
install.packages("randomForest")
library(randomForest) #both bagging trees and random forest use this library
set.seed(1156)
emsns <-Emsns
train <- sample(1:nrow(emsns), nrow(emsns)/2) #dividing hitters data
#into training and test sets

ncol(emsns)
bag.emsns <- randomForest(CO2Emissions ~., subset = train, data = emsns, mtry = 6)
dim(emsns)

# Predictions on the test set
test.salary <- emsns[-train, 6]
yhat.bag <- predict(bag.emsns, newdata = emsns[-train,])
cbind(yhat.bag, test.CO2Emissions)[1:10,] 
mean((yhat.bag - test.CO2Emissions)^2)  


# variable importance plot
varImpPlot(bag.emsns) #tells us which predictors were used more in prediction
#"chmrun" was the most important predictor/used the most for the bagged trees
#and reduced the RSS by the most [?]. "division" least so


####################################
# Random Forests
# By default in R, m = p/3 for random forest of regression trees
# By default in R, m = \sqrt{p} for random forest of classification trees
rf.emsns <- randomForest(CO2Emissions ~., subset = train, data = emsns, mtry = 6)
#^for every split, we choose 6 predictors; that's how we decorrelated the trees

# Predictions on the test data set
yhat.rf <- predict(rf.emsns, newdata = emsns[-train,])
cbind(yhat.rf, test.CO2Emissions)[1:10,]
mean((yhat.rf - test.CO2Emissions)^2) #MSE improved a bit over bagging = 62,730

# variable importance plot
varImpPlot(rf.emsns) #almost the same as for bagging


