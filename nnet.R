# Last Name: Attri
# First Name: Siddhant
# Student ID: 1001382754
# Assignment: Assignment on advanced 

# First we will import dataset from https://archive.ics.uci.edu/ml/machine-learning-databases/00242/
## Since it was giving me error to import the .xlsx, so I converted the dataset file to .csv and then 
#imported it  
#It has 8 attributes denoted by (X1..X8) and two responses(Y1,Y2).The aim is to use the eight features 
#to predict each of the responses
df <- ENB2012_data
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
# The import adds to number of empty observations at the the end of data frame, so this
# command will remove them
df <- df[complete.cases(df),]
# 2 of the attributes, X6 and X8 are actually factors, for the nueral network to work,
# we will convert them to dummy variables.
library(caret)
df$X6 <- factor(df$X6)
df$X8 <- factor(df$X8)
dummies <- dummyVars(Y1 + Y2 ~ ., data = df)
df_data <- cbind(as.data.frame(predict(dummies, newdata = df)), df[,9:10])
dim(df_data)
# now new binary columns have been produced
# many columns created a factor as there are levels in the factor.
# now we will split the dataset in 80:20 ratio between training and testing data
set.seed(474576)
df_sample_vector <- createDataPartition(df_data$Y1, p = 0.80, list = FALSE)
df_train <- df_data[df_sample_vector, 1:16]
df_train_output <- df_data[df_sample_vector, 17:18]
df_test <- df_data[-df_sample_vector, 1:16]
df_test_output <- df_data[-df_sample_vector, 17:18]
# now we will scale all the data dimensions to the unit interval, noting this does not 
# effect the binary columns created earlier. Pre-processing will be done
df_pp <- preProcess(df_train, method = c("range"))
df_train_pp <- predict(df_pp, df_train)
df_test_pp <- predict(df_pp, df_test)
df_train_out_pp <- preProcess(df_train_output, method = c("range"))
df_train_output_pp <- predict(df_train_out_pp, df_train_output)
df_test_output_pp <- predict(df_train_out_pp,df_test_output)
# we will use nueralnet library to create nueral network model
library(neuralnet)
n <- names(df_data)
f <- as.formula(paste("Y1 + Y2 ~", paste(n[!n %in% c("Y1", "Y2")], collapse = " + "))) 
df_model <- neuralnet(f, data = cbind(df_train_pp, df_train_output_pp), hidden = 10)
plot(df_model)


