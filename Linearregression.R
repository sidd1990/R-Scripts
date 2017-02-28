# Last Name: Attri
# First Name: Siddhant
# Student ID: 1001382754
# Assignment: Assignment # 6  

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
# 
dataset1[,1] <- as.numeric(dataset1[,1])
dataset1[,2] <- as.numeric(dataset1[,2])
dataset1[,3] <- as.numeric(dataset1[,3])
dataset1[,4] <- as.numeric(dataset1[,4])
dataset1[,5] <- as.numeric(dataset1[,5])
dataset1[,6] <- as.numeric(dataset1[,6])
dataset1[,7] <- as.numeric(dataset1[,7])
dataset1[,8] <- as.numeric(dataset1[,8])
dataset1[,9] <- as.numeric(dataset1[,9])
attach(dataset1)
# We will plot Y1 and Y2 and find out the relationship between them. 
plot(Y1,Y2)
# After the plotting we find out that there is a linear relationship between Y1(heating load)
# and Y2(Cooling load). Hence we will use Y1 as response variable and the rest variables as predictors

par(mfrow=c(2,4))
plot(dataset1$X1,dataset1$Y1)
plot(dataset1$X2,dataset1$Y1)
plot(dataset1$X3,dataset1$Y1)
plot(dataset1$X4,dataset1$Y1)
plot(dataset1$X5,dataset1$Y1)
plot(dataset1$X6,dataset1$Y1)
plot(dataset1$X7,dataset1$Y1)
plot(dataset1$X8,dataset1$Y1)

cor.val <- cor(dataset1[,1:8],dataset1[,9],method = "spearman")
#[,1]
#X1  0.551576560
#X2 -0.551576560
#X3  0.431626969
#X4 -0.719495212
#X5  0.771071320
#X6 -0.004163071
#X7  0.169150668
#X8 -0.080635524

#Removing X6 and X8 since they dont have correlation with Y1
str(dataset1)

# lets Split the datset to Train and Test set in the ratio 80:20
set.seed(123)
data_samp <- sample.int(nrow(dataset1),as.integer(nrow(dataset1)*0.2),replace = F)
dataTest <- dataset1[data_samp,]
dataTrain <- dataset1[-data_samp,]
attach(dataTrain)

Test <- data.frame(cbind(X1,X2,X3,X4,X5,X7,Y1))
lm.mo <- lm(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, data = dataTrain)
summary(lm.mo)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-147.75  -55.06  -13.13   28.61  531.03 

#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    -703.139    100.142  -7.021 5.88e-12 ***
#  X1            26.945     15.796   1.706  0.08856 .  
#  X2                NA         NA      NA       NA    
#  X3            53.246     16.179   3.291  0.00106 ** 
#  X4            36.844     19.041   1.935  0.05345 .  
#X5           111.041     80.632   1.377  0.16898    
#X7            22.851      4.361   5.239 2.22e-07 ***
  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#Residual standard error: 100.6 on 609 degrees of freedom
#Multiple R-squared:  0.6495,	Adjusted R-squared:  0.6466 
#F-statistic: 225.7 on 5 and 609 DF,  p-value: < 2.2e-16
par(mfrow=c(2,2))  
plot(lm.mo)

# The residuals vs Fitted values graph shows that the variables are scattered on the both side of the  
# reference line. Also there is a very high variance in the values. This is due to the fact that most  
# variables have few distinct values because of which the values in graph are scattered. 

#As the p-value is much less than 0.05, we reject the null hypothesis that β = 0. Hence there is a 
#significantrelationship between the variables in the linear regression model of the data set. 
# we can also compare the two models
lm.mo1 <- lm(Y1 ~ X1 + X2)
anova(lm.mo,lm.mo1)
#Model 1: Y1 ~ X1 + X2 + X3 + X4 + X5 + X7
#Model 2: Y1 ~ X1 + X2
#Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
#1    591  5182                                  
#2    594 31110 -3    -25928 985.67 < 2.2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1$ 
#==================================================================================================
# Stepwise Regression
step(lm.mo,direction = "backward")
#Start:  AIC=1302.13
#Y1 ~ X1 + X2 + X3 + X4 + X5 + X7


#Step:  AIC=1302.13
#Y1 ~ X1 + X2 + X3 + X5 + X7

#Df Sum of Sq    RSS    AIC
#<none>              5182.0 1302.1
#- X2    1     191.5 5373.5 1321.8
#- X1    1     282.0 5464.0 1331.8
#- X3    1     591.3 5773.3 1364.6
#- X5    1     993.1 6175.1 1404.8
#- X7    1    4589.2 9771.2 1678.8

#Call:
#  lm(formula = Y1 ~ X1 + X2 + X3 + X5 + X7, data = dataTrain)

#Coefficients:
#  (Intercept)           X1           X2           X3           X5           X7  
#86.60738    -65.35078     -0.08942      0.06091      4.07397     20.54629  
# During each step the attribute with the lowest p-value is removed. The AIC of the model increases when the attribute with the lowest p-value
# is removed. After removing X4 the AIC remains unchanged. This happens due to the fact that X4 does not play a part in determining the Y1. We
# can see that X1,X2,X3,X5,X7 play a significant role in deciding Y1.


#==============================================================================================
#Decision Tree for this data set
library(rpart)
library(rpart.plot)
fit <- rpart(Y1 ~ X1 + X2 + X3 + X4 + X5 + X7 , data = dataTest, method = "class")
printcp(fit)
#Classification tree:
#  rpart(formula = Y1 ~ X1 + X2 + X3 + X4 + X5 + X7, data = dataTest, 
#        method = "class")

#Variables actually used in tree construction:
#  [1] X1

#Root node error: 169/171 = 0.9883

#n=171 (88 observations deleted due to missingness)

#CP nsplit rel error xerror xstd
#1 0.011834      0    1.0000 1.0118    0
#2 0.010000      3    0.9645 1.0118    0

summary(fit)
predict(fit)
plot(fit, uniform=TRUE, margin = 0.6,branch=0.6, compress = TRUE, main="Tree for Energy Efficiency dataset")
text(fit, use.n = TRUE, cex=0.7,all=TRUE, pretty = 1)

