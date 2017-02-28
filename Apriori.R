# Last Name: Attri
# First Name: Siddhant
# Student ID: 1001382754
# Assignment: Assignment # 5  

#First, we imported our selected dataset from url: http://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data
#Now, installing arules and arulesViz libraries.

library(arules)
library(arulesViz)

# The data is without columns, therefore, specifying columns of the data from the given repository.
colnames(adult) <- c("Age", "Workclass", "fnlwgt", "Education", "Education-num", "Marital-status", "Occupation", "Relationship", "Race", "Sex", "Capital-gain", "Capital-loss", "hours-per-week", "Native-country", "Salary")

# Attributes such as fnlwgt and education-num are not to be used, hence, they can be removed
adult[["fnlwgt"]] = NULL
adult[["Education-num"]] = NULL

#We should Map the the different values of the attributes to make it more easier.
#First, it will be done for "Age"
adult[["Age"]] <- ordered(cut(adult[["Age"]], c(15,25,45,65,100)), labels("Young", "Middle-Aged", "Senior", "Old"))

#Similarly for hours-per-week
adult[["hours-per-week"]] <- ordered(cut(adult[["hours-per-week"]], c(0,25,40,60,100)), labels("Part-time", "Full-Time", "Over-time", "Workaholic"))

#Similarly for capital gain
adult[["Capital-gain"]] <- ordered(cut(adult[["Capital-gain"]], c(-Inf,0,median(adult[["Capital-gain"]][adult[["Capital-gain"]]>0]), Inf)), labels("None", "Low", "High"))

#For Capital-loss
adult[["Capital-loss"]] <- ordered(cut(adult[["Capital-loss"]], c(-Inf,0,median(adult[["Capital-loss"]][adult[["Capital-loss"]]>0]), Inf)), labels("None", "Low", "High"))

#Create Transactions
adult <- as(adult, "transactions")

# Mine Association rules
rules <- apriori(adult, parameter = list(support = 0.4, conf = 0.5, target = "rules"))

#Sort the rules
rules.sorted <- sort(rules, by="lift") 
inspect(rules.sorted)

# Above results show 67 rules and out of which some rules may be redundant

# Finding Redundant Rules
subset.matrix <- is.subset(rules.sorted, rules.sorted)
subset.matrix[lower.tri(subset.matrix, diag = T)] <- NA
Redundant <- colSums(subset.matrix, na.rm = T) >= 1
which(Redundant)

#{Marital-status= Married-civ-spouse,Relationship= Husband} 
#3 
#{Marital-status= Married-civ-spouse,Relationship= Husband,Sex= Male} 
#4 
#{Relationship= Husband,Sex= Male} 
#6 
#{Marital-status= Married-civ-spouse,Relationship= Husband,Sex= Male} 
#7 
#{Marital-status= Married-civ-spouse,Sex= Male} 
#9 
#{Race= White,Sex= Male} 
#15 
#{Race= White,Sex= Male,Native-country= United-States} 
#16 
#{Workclass= Private,Salary= <=50K} 
#18 
#{Race= White,Native-country= United-States} 
#20 
#{Workclass= Private,Race= White,Sex= Male} 
#21 
#{Race= White,Sex= Male,Native-country= United-States} 
#22 
#{Workclass= Private,Race= White,Native-country= United-States,Salary= <=50K} 
#23 
#{Race= White,Native-country= United-States,Salary= <=50K} 
#24 
#{Workclass= Private,Native-country= United-States,Salary= <=50K} 
#25 
#{Workclass= Private,Race= White,Native-country= United-States} 
#26 
#{Workclass= Private,Native-country= United-States,Salary= <=50K} 
#27 
#{Race= White,Sex= Male,Salary= <=50K} 
#28 
#{Workclass= Private,Race= White,Sex= Male} 
#29 
#{Workclass= Private,Race= White,Native-country= United-States,Salary= <=50K} 
#30 
#{Workclass= Private,Race= White,Native-country= United-States,Salary= <=50K} 
#31 
#{Workclass= Private,Race= White,Salary= <=50K} 
#32 
#{Race= White,Native-country= United-States,Salary= <=50K} 
#33 
#{Workclass= Private,Race= White} 
#35 
#{Workclass= Private,Race= White,Native-country= United-States,Salary= <=50K} 
#41 
#{Sex= Male,Native-country= United-States} 
#42 
#{Sex= Male,Native-country= United-States} 
#43 
#{Marital-status= Married-civ-spouse,Native-country= United-States} 
#44 
#{Native-country= United-States,Salary= <=50K} 
#45 
#{Native-country= United-States,Salary= <=50K} 
#46 
#{Workclass= Private,Race= White,Native-country= United-States} 
#47 
#{Workclass= Private,Native-country= United-States} 
#48 
#{Workclass= Private,Native-country= United-States} 
#49 
#{Sex= Male,Native-country= United-States,Salary= <=50K} 
#50 
#{Workclass= Private,Sex= Male,Native-country= United-States} 
#51 
#{Workclass= Private,Sex= Male} 
#52 
#{Workclass= Private,Sex= Male} 
#53 
#{Workclass= Private,Race= White,Sex= Male} 
#54 
#{Workclass= Private,Race= White,Salary= <=50K} 
#55 
#{Workclass= Private,Native-country= United-States,Salary= <=50K} 
#56 
#{Workclass= Private,Sex= Male,Native-country= United-States} 
#57 
#{Race= White,Salary= <=50K} 
#58 
#{Race= White,Salary= <=50K} 
#59 
#{Race= White,Native-country= United-States,Salary= <=50K} 
#60 
#{Workclass= Private,Sex= Male,Native-country= United-States} 
#61 
#{Race= White,Sex= Male,Salary= <=50K} 
#62 
#{Sex= Male,Salary= <=50K} 
#63 
#{Sex= Male,Salary= <=50K} 
#64 
#{Sex= Male,Native-country= United-States,Salary= <=50K} 
#65 
#{Sex= Male,Native-country= United-States,Salary= <=50K} 
#66 
#{Race= White,Sex= Male,Salary= <=50K} 
#67 


# Now rules after removing redundant rules
FinalRules <- rules.sorted[!Redundant]
inspect(FinalRules)
# There are 50 redundant rules

# Now drawing the scatter plot of 17 rules 
plot(FinalRules)
# Now a graph plot for 17 rules
plot(FinalRules, method="graph", control = list(type="items"))
# Parallel coordinates plot for 17 rules
plot(FinalRules, method = "paracoord", control = list(reorder=TRUE))
# Matrix plor for 17 rules
plot(FinalRules, method = "matrix", measure = "lift")
# Matrix 3D plot for 17 rules
plot(FinalRules, method = "matrix3D", measure = "lift")

