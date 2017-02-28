# Last Name: Attri
# First Name: Siddhant
# Student ID: 1001382754
# Assignment: Assignment on advanced 

# First we will import the dataset from th ML repository called the energy efficiency dataset
# the data set can be downloaded from https://archive.ics.uci.edu/ml/machine-learning-databases/00242/
# Since it was giving me error to import the .xlsx, so I converted the dataset file to .csv and then 
#imported it  
#It has 8 attributes denoted by (X1..X8) and two responses(Y1,Y2).The aim is to use the eight features 
#to predict each of the responses
dataset1 <- ENB2012_data
#X1	Relative Compactness 
#X2	Surface Area 
#X3	Wall Area 
#X4	Roof Area 
#X5	Overall Height 
#X6	Orientation 
#X7	Glazing Area 
#X8	Glazing Area Distribution 
#Y1	Heating Load 
#Y2	Cooling Load
#While importing dataset, there were extra attributes named "X" and "X.1"
#therfore we will delete them.
dataset1[["X"]] = NULL
dataset1[["X.1"]] = NULL
dataset1[,1] <- as.numeric(dataset1[,1])
dataset1[,2] <- as.numeric(dataset1[,2])
dataset1[,3] <- as.numeric(dataset1[,3])
dataset1[,4] <- as.numeric(dataset1[,4])
dataset1[,5] <- as.numeric(dataset1[,5])
dataset1[,6] <- as.numeric(dataset1[,6])
dataset1[,7] <- as.numeric(dataset1[,7])
dataset1[,8] <- as.numeric(dataset1[,8])
dataset1[,9] <- as.numeric(dataset1[,9])
#Splitting data set to Train and Test set in the ratio 80:20
set.seed(123)
samp <- sample.int(nrow(dataset1),as.integer(nrow(dataset1)*0.2),replace = F)
test <- dataset1[samp,]
train <- dataset1[-samp,]
xtest <- test[,1:6]
ytest <- test[,7]
attach(test)
#
library(arm)
fit.bayes <- bayesglm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7,family=gaussian(link=identity),data=train,prior.df = Inf,prior.mean = 0,prior.scale = NULL,maxit = 10000)
ypred.bayes <- predict.glm(fit.bayes,newdata = xtest,se.fit = T)
ypred.bayes$fit
display(fit.bayes)
#bayesglm(formula = Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, family = gaussian(link = identity), 
#         data = train, prior.mean = 0, prior.scale = NULL, prior.df = Inf, 
#         maxit = 10000)
#coef.est coef.se
#(Intercept)  80.73    20.84 
#X1          -62.13    11.24 
#X2           -0.06    20.64 
#X3            0.04    20.64 
#X4           -0.05    41.28 
#X5            4.14     0.38 
#X7           20.54     0.90 
#---
#  n = 597, k = 7
#residual deviance = 5182.7, null deviance = 60950.6 (difference = 55768.0)
#overdispersion parameter = 8.8
#residual sd is sqrt(overdispersion) = 2.96
plot(ypred.bayes$fit)

