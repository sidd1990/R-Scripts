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
df<- ENB2012_data
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
df[["X"]] = NULL
df[["X.1"]] = NULL
ds <- df[complete.cases(df),]
# let us split the dataset in 80:20 ratio of train and test
set.seed(123)
data_samp <- sample.int(nrow(ds),as.integer(nrow(ds)*0.2),replace = F)
dataTest <- ds[data_samp,]
dataTrain <- ds[-data_samp,]
svmft <- svm(factor(Y1) ~ X1 + X2, data = dataTrain, type = "C-classification")
print(svmft)
#Call:
#  svm(formula = factor(Y1) ~ X1 + X2, data = dataTrain, type = "C-classification")


#Parameters:
#  SVM-Type:  C-classification 
#SVM-Kernel:  radial 
#cost:  1 
#gamma:  0.5 

#Number of Support Vectors:  326
#We will divide 
Z1 <- which(dataTrain$Y1 <= 10)
dataTrain$Y1[Z1] <- "Low hot"
Z1 <- which(dataTrain$Y1 > 10 && dataTrain$Y1 <= 30)
dataTrain$Y1[Z1] <- "Medium hot"
Z1 <- which(dataTrain$Y1 > 30 && dataTrain$Y1 <= 47)
dataTrain$Y1[Z1] <- "High hot"
predict(svmft, dataTrain)
plot(svmft, dataTrain, X1~X2)
# Here we have taken plot of X1 and X2, for Y1. However, other attributes can also be computed for Y1 but X1 and X2 gave better results. 


