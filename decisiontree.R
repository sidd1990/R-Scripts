#First, we imported our selected dataset from url: http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
adult <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data", header = F)
# The data is without columns, therefore, specifying columns of the data from the given repository.
colnames(adult) <- c("age", "workclass", "fnlwgt", "education", "educationnum", "maritalstatus", "occupation", "relationship", "race", "sex", "capitalgain", "capitalloss", "hoursperweek", "nativecountry", "salary")
# Attributes such as fnlwgt and education-num are not to be used, hence, they can be removed
adult[["fnlwgt"]] = NULL
adult[["Education-num"]] = NULL
library(rpart)
library(rpart.plot)
library(party)
#Use fit formula for constructing the decision tree. 

fit <- rpart(salary ~ age + sex + education + occupation , method = "class", data = adult)
#display the results, and summary of the splits 
printcp(fit)
plotcp(fit)
summary(fit)
predict(fit)
# plot the decision tree 
plot(fit, uniform=TRUE, margin = 0.1, compress = TRUE, main="Classification tree for Adult dataset")
text(fit, use.n = TRUE, all = TRUE, cex=0.7, pretty = 1)

