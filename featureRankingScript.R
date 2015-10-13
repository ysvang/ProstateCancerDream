#' featureRankSCript
#' 
#' Feature selection involves running this script to calcuates the concordance index for each covariates and 
#' ranks them in descending order.  Top 20 features are used for modeling
#' 
#' Author: Yeeleng Scott Vang (ysvang@uci.edu)


rm(list=ls()) # clear all variables from environment
source('prostateCancerFunctions.R')

set.seed(15)

# builds an object containing training and validation data
data <- readData("trainDesignMatrix.csv", "testDesignMatrix.csv")
# normalize data object
data.norm <- normalizeData(data, 12) 
# extract just the training data frame
trainingDF <- data.norm$trainingDF 

# store covariate name in a list
columnNames <- colnames(trainingDF)
# initialize vector to hold cIndex for each covariate
c_index <- c(rep(0,length(columnNames)))

# create a survival object
surv <- Surv(data.norm$time, data.norm$event)

# iterate through all covariate and calcualte their cIndex
for (i in 1:ncol(trainingDF)){
  sum.surv <- summary(coxph(surv ~ trainingDF[,i]))
  c_index[i] <- sum.surv$concordance[1]
}

# pair covariate name with it's cooresponding cIndex and sort
c_index_ranking <- data.frame(columnNames, c_index)
ordered_c_index <- c_index_ranking[with(c_index_ranking, order(-c_index)),]

# write out only the top 20 covariate to file
write.csv(ordered_c_index[1:20,], file = "ranked_features.csv", row.names=FALSE)