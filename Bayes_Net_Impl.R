# Q4: put your code implementation for Q4 in this file and put proper comments for
# each line of your code. 
require(bnlearn)
#This code is for reading the data from bn-data.csv
bayesData <- read.csv("bn-data.csv")

#This code will extract the data of the attributes that are used for generating the Bayes Net
bData <- subset(bayesData,select=c(2,3,4,5,6,7))

#This Code will fit the data to create Bayes Net.
fit <- hc(bData)

#This code plots the Bayes Net.
plot(fit)

#This code prints the Conditional Probability Tables for  each attribute.
bn.fit(fit,bData)