#' prostateCancerMain
#' 
#' This script performs the training and ensembling
#' 
#' Author: Yeeleng Scott Vang (ysvang@uci.edu)

rm(list=ls()) # clear all variables from environment
source("prostateCancerFunctions.R")
source("score.R")


set.seed(5) # set random generator seed

# read in training design matrix, time, and event
trainDF <- read.csv("trainDesignMatrix.csv")
time <- read.csv("trainTime.csv")
names(time) <- c("time")
time <- as.matrix(time) # requires time to be a matrix to work with Surv
event <- read.csv("trainEvent.csv")
names(event) <- c("event")
event <- as.matrix(event)  # requires event to be a matrix to work with Surv

# read in test/validation design matrix
testDF <- read.csv("testDesignMatrix.csv")

# Top 20 covariates from feature selection step.  Ordered here in such a way that real-valued features on the left-most columns,
# and binary-valued features are on the right-most columns
top20Features <- c("AST", "CA", "BMI", "NEU", "ALP", "HB", "PSA", 
                   "ECOG_C", "ANALGESICS", "TARGET", "LIVER", "tDF.WGTBLCAT..70.80", "MHCARD", 
                   "tDF.WGTBLCAT..60.70", "GONADOTROPIN", "tDF.AGEGRP265.74", "BONE",
                   "PROSTATECTOMY", "tDF.AGEGRP2..75", "MHGEN")

trainDF <- trainDF[, top20Features]
testDF <- testDF[, top20Features]

# q-normalize covariates ALP, HB, and PSA in both training and test dataframe
trainDF$ALP <- MapToNorm(trainDF$ALP)
trainDF$HB <- MapToNorm(trainDF$HB)
trainDF$PSA <- MapToNorm(trainDF$PSA)
testDF$ALP <- MapToNorm(testDF$ALP)
testDF$HB <- MapToNorm(testDF$HB)
testDF$PSA <- MapToNorm(testDF$PSA)

# builds the data object consisting of training and test data
dataObject <- list("time" = time, "event" = event, "trainingDF" = trainDF, "testDF" = testDF)

# normalize the real-valued covariates in the training and test dataframe
dataObjectNorm <- normalizeData(dataObject, 4)

# make prediction using coxph  
modcoxph <- model.coxph(dataObjectNorm)

# make prediction using gbm with distribution 'coxph'
modgbm <- model.gbmcoxph(dataObjectNorm, 1500, 5, .8, .002)

# make prediction using gbm with distribution 'sci'
modgbmci<- model.gbmci(dataObjectNorm, 2000, 2, .8, 1)


# scale the columns of the predicted risk/survival probabilities by the sum of each column  
modcoxph.scaled <- scale(modcoxph, center=F, scale=colSums(modcoxph))
modgbm.scaled <- scale(modgbm, center=F, scale=colSums(modgbm))
modgbmci.scaled <- -scale(modgbmci, center=F, scale=colSums(modgbmci))


# ensemble the three models together
ensemble <- .6*modcoxph.scaled + .2*modgbm.scaled +  .2*modgbmci.scaled

testTable <- read.csv("final\\CoreTable_validation.csv")
submittalDF <- data.frame(testTable['RPT'], ensemble[,1], -ensemble[,2], -ensemble[,3], -ensemble[,4])
names(submittalDF)[2] <- "riskScoreGlobal"
names(submittalDF)[3] <- "riskScore12"
names(submittalDF)[4] <- "riskScore18"
names(submittalDF)[5] <- "riskScore24"
# write to CSV output
write.csv(submittalDF, file = "Question1A_Final.csv", row.names=FALSE)



