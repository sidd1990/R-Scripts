# 2.2 Suppose that the data for analysis includes the attribute age. The age values for the data tuples are (in increasing order) 13, 15, 16, 16, 19, 20, 20, 21, 22, 22, 25, 25, 25, 25, 30, 33, 33, 35, 35, 35, 35, 36, 40, 45, 46, 52, 70. 
# (a) What is the mean of the data? What is the median? 
# Input the numbers
x <- c(13,15,16,16,19,20,20,21,22,22,25,25,25,25,30,33,33,35,35,35,35,36,40,45,46,52,70)
# Calculate mean
result.mean <- mean(x)
# print mean
print(result.mean)
#Calculate Median
result.median <- median(x)
# Print median
print(result.median)
# (b) What is the mode of the data? Comment on the data’s modality (i.e., bimodal, trimodal, etc.) 
# Create the function for mode because R does not have a pre-defined function for it
mode <- function(x) {uniqx <- unique(x)                             
+ + uniqx[which.max(tabulate(match(x,uniqx)))]}
print(result.mode)
# (c) What is the midrange of the data? 
# Assign maximum number
y <- max(x)
#Assign minimum number
z <- min(x)
#Take Average of maximum and minimum number
midrange <- (y+z)/2
# Print midrange
print(midrange)
# (d) Can you find (roughly) the first quartile (Q1) and the third quartile (Q3) of the data?
# to show the quantile of this data
quantile(x)
summary(x)
# (e) Give the five-number summary of the data.
# To show the five number summary
fivenum(x)

# 2.3 Make a data table for grouped data
agedata <- data.table(lowage = c(1, 6, 16, 21, 51, 81), highage = c(5, 15, 20, 50, 80, 110), frequencies = c(200, 450, 300, 1500, 700, 44)
)
# Add column for cumulative frequencies 
agedata <- agedata[,cumnumbers := cumsum(frequencies)]
#identifying median group
mediangroup <- agedata[
  (cumnumbers - frequencies) <= (max(cumnumbers)/2) & 
    cumnumbers >= (max(cumnumbers)/2)]
mediangroup

#creating the variables needed to calculate median
mediangroup[,l := lowage]
mediangroup[,h := highage - lowage]
mediangroup[,f := frequencies]
mediangroup[,c := cumnumbers- frequencies]
n = agedata[,sum(frequencies)]

#calculating median
median <- mediangroup[,l + ((h/f)*((n/2)-c))]
median

# 2.6 (a) # This function computes and returns the distance matrix computed by using the specified distance measure to compute the distances between the rows of a data matrix.
t <- matrix(c(22, 1, 42, 10, 20, 0, 36, 8), nrow = 2, ncol = 4, byrow = TRUE)
# t = matrix
print(t)
# Find the euclidean distance
dist(t, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
# (b) Similarly find manhattan distance
dist(t, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
# (c) Similarly find Minkowski distance with q = 3
dist(t, method = "minkowski", diag = FALSE, upper = FALSE, p = 3)
# (d) to find supremum distance
dist(t, method = "maximum", diag = FALSE, upper = FALSE, p = 2)

# 2.8 a) Consider the data as 2-D data points. 
# Given a new data point, x = (1.4,1.6) as a query, rank the database points based on similarity with the query using Euclidean distance, Manhattan distance, supremum distance, and cosine similarity.
# Put the values of each datapoint with x
x1 <- matrix(c(1.5,1.7,1.4,1.6), nrow = 2, ncol = 2, byrow = TRUE)
x2 <- matrix(c(2.0,1.9,1.4,1.6), nrow = 2, ncol = 2, byrow = TRUE)
x3 <- matrix(c(1.6,1.8,1.4,1.6), nrow = 2, ncol = 2, byrow = TRUE)
x4 <- matrix(c(1.2,1.5,1.4,1.6), nrow = 2, ncol = 2, byrow = TRUE)
x5 <- matrix(c(1.5,1.0,1.4,1.6), nrow = 2, ncol = 2, byrow = TRUE)

# Find Euclidean Distance for each of them from data point x
ex1 <- dist(x1, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
ex2 <- dist(x2, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
ex3 <- dist(x3, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
ex4 <- dist(x4, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
ex5 <- dist(x5, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

# Form data table comprising of all data points and their respective euclidean ditance
eucdata <- data.table(Ranks = c("x1", "x2", "x3", "x4", "x5"), EuclideanDistance = c(ex1,ex2,ex3,ex4,ex5))

# Sort data table in decreasing order according to euclidean distance
eucldata <- eucdata[order(EuclideanDistance),] 
print(eucldata)

# Find Manhattan distance for each of the data points from data point x
mx1 <- dist(x1, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
mx2 <- dist(x2, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
mx3 <- dist(x3, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
mx4 <- dist(x4, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)
mx5 <- dist(x5, method = "manhattan", diag = FALSE, upper = FALSE, p = 2)

# Form data table comprising of data points and their respective manhattan distance
mdata <- data.table(Ranks = c("x1", "x2", "x3", "x4", "x5"), ManhattanDistance = c(mx1,mx2,mx3,mx4,mx5))

# Sort data table in increasing order according to Manhattan distance to show dissimilarity
mandata <- mdata[order(ManhattanDistance),] 
print(mandata)

# Find Supremum Distance of each of the data points from data points x
sx1 <- max(abs((x1[1, ] - x1[2, ])))
sx2 <- max(abs((x2[1, ] - x2[2, ])))
sx3 <- max(abs((x3[1, ] - x3[2, ])))
sx4 <- max(abs((x4[1, ] - x4[2, ])))
sx5 <- max(abs((x5[1, ] - x5[2, ])))

# Form data table comprising of data points and their respective supremum distance
sdata <- data.table(Ranks = c("x1", "x2", "x3", "x4", "x5"), SupremumDistance = c(sx1,sx2,sx3,sx4,sx5))
# Sort data table in increasing order to find ranks of data points to show dissimilarity
supdata <- sdata[order(SupremumDistance),] 
print(supdata)

# Find the cosine measure of each data point but for that we have to install "proxy package" and then call library package
install.packages('proxy')
library(proxy)
cx1 <- dist(x1, method = "cosine", diag = FALSE, upper = FALSE, p = 2)
cx2 <- dist(x2, method = "cosine", diag = FALSE, upper = FALSE, p = 2)
cx3 <- dist(x3, method = "cosine", diag = FALSE, upper = FALSE, p = 2)
cx4 <- dist(x4, method = "cosine", diag = FALSE, upper = FALSE, p = 2)
cx5 <- dist(x5, method = "cosine", diag = FALSE, upper = FALSE, p = 2)

# Form data table comprising of data points and their respective cosine measure
cdata <- data.table(Ranks = c("x1", "x2", "x3", "x4", "x5"), CosineMeasure = c(cx1,cx2,cx3,cx4,cx5))

# Sort the data table in decreasing order to show similarity
cosdata <- cdata[order(CosineMeasure),]
print(cosdata)

# To normalize the data, First create a function to normalize each matrix
normfunc <- function(x) {x / sqrt(sum(x^2))}
# Normalize each matrix above with the function defined
normx1 <- normfunc(x1)
normx2 <- normfunc(x2)
normx3 <- normfunc(x3)
normx4 <- normfunc(x4)
normx5 <- normfunc(x5)

# Find Euclidean Distance for each of normalized data point from data point x
enormx1 <- dist(normx1, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
enormx2 <- dist(normx2, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
enormx3 <- dist(normx3, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
enormx4 <- dist(normx4, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)
enormx5 <- dist(normx5, method = "euclidean", diag = FALSE, upper = FALSE, p = 2)

# Form data table comprising of all data points and their respective euclidean ditance
NormEucData <- data.table(Ranks = c("x1", "x2", "x3", "x4", "x5"), NormalisedEuclideanDistance = c(enormx1,enormx2,enormx3,enormx4,enormx5))

# sort the data table in increasing order to show similarity
SortedNormEucData <- NormEucData[order(NormalisedEuclideanDistance),]
print(SortedNormEucData)

