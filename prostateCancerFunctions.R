library(survival)
library(pec)
library(MASS)
library(muhaz)
library(caret)
library(matrixStats)
library(gbm,lib.loc ='gbmci')
library(glmnet)


# read in training and testing data
readData <- function(trainFile, testFile) {

    trainingDF <- read.csv(trainFile)
    testDF <- read.csv(testFile)
  
    time <- read.csv("trainTime.csv")
    names(time) <- c("time")
    time <- as.matrix(time) # requires time to be a matrix to work with Surv
    event <- read.csv("trainEvent.csv")
    names(event) <- c("event")
    event <- as.matrix(event)  # requires event to be a matrix to work with Surv
    return(list("time" = time, "event" = event, "trainingDF" = trainingDF, "testDF" = testDF))
}

# map data vector x to norm based on quantile 
MapToNorm <- function(x){
  return (qnorm(ppoints(length(x)))[order(order(x))])
}



# normalize real-valued columns
normalizeData <- function(dataObject, n){
    
    time <- dataObject$time
    trainingDF <- dataObject$trainingDF
    event <- dataObject$event
    testDF <- dataObject$testDF
    
    preProc  <- preProcess(trainingDF[,1:n], method=c("center", "scale")) # calculate mean and variance of training set
    trainingDF.norm <- predict(preProc, trainingDF[,1:n]) # apply centering/scaling to training set
    trainingDF.norm = data.frame(trainingDF.norm, trainingDF[,(n+1):ncol(trainingDF)])
    testDF.norm     <- predict(preProc, testDF[,1:n])  # apply centering/scaling to test set
    testDF.norm = data.frame(testDF.norm, testDF[,(n+1):ncol(testDF)])
    return(list("time" = time, "event" = event, "trainingDF" = trainingDF.norm, "testDF" = testDF.norm))
}


# coxph regression
model.coxph <- function(dataObject){
    coxfit <- coxph(formula=Surv(dataObject$time, dataObject$event) ~ ., data=dataObject$trainingDF, method="efron")
    globalRisk <- predict(coxfit, newdata=dataObject$testDF, type="risk")  # global (relative risk)
    # predict survival probability (risk) at 12, 18, 24 months
    testSurvivalProb <- predictSurvProb(coxfit, newdata=dataObject$testDF, times=seq(366,732,183)) 
    risksDF <- data.frame(globalRisk, testSurvivalProb[,1], testSurvivalProb[,2], testSurvivalProb[,3])
    return(risksDF)
}



# gradient boost machine with coxph
model.gbmcoxph <- function(dataObject, numTrees, depth, bagFraction, shrink){

    time <- dataObject$time
    event <- dataObject$event
    trainingDF <- dataObject$trainingDF
    testDF <- dataObject$testDF
    
    coxphfit.gbm <- gbm(Surv(time, event) ~ ., distribution = "coxph", n.trees = numTrees, data = trainingDF, shrinkage= shrink,
                        interaction.depth = depth, bag.fraction = bagFraction, cv.folds = 5, keep.data = TRUE, verbose = FALSE)
    
    best.iter <- gbm.perf(coxphfit.gbm, method = "cv", plot.it=FALSE)   # returns test set estimate of best number of trees
    cumulativeHaz <- basehaz.gbm(time, event, coxphfit.gbm$fit, t.eval = seq(366, 732, 183), cumulative = TRUE)
    testDF.linPred <- predict(coxphfit.gbm, newdata = testDF, best.iter)
    globalRisk <- exp(testDF.linPred) # global risk
    s.12mon <- exp(-cumulativeHaz[1])^globalRisk # Survival prob at 12 months
    s.18mon <- exp(-cumulativeHaz[2])^globalRisk # Survival prob at 18 months
    s.24mon <- exp(-cumulativeHaz[3])^globalRisk # Survival prob at 24 months
    risksDF <- data.frame(globalRisk, s.12mon, s.18mon, s.24mon)
    return(risksDF)    
}

# gradient boost machine with c-index optimization
model.gbmci <- function(dataObject, numTrees, depth, bagFraction, shrink){
  
  time <- dataObject$time
  event <- dataObject$event
  trainingDF <- dataObject$trainingDF
  testDF <- dataObject$testDF
  
  coxphfit.gbm <- gbm(Surv(time, event) ~ ., distribution = "sci", n.trees = numTrees, data = trainingDF, 
                      interaction.depth = depth, bag.fraction = bagFraction, cv.folds = 5, keep.data = TRUE, verbose = FALSE, shrinkage = shrink)
  best.iter <- gbm.perf(coxphfit.gbm, method = "cv", plot.it=FALSE)   # returns test set estimate of best number of trees
  cumulativeHaz <- basehaz.gbm(time, event, coxphfit.gbm$fit, t.eval = seq(366, 732, 183), cumulative = TRUE)
  testDF.linPred <- predict(coxphfit.gbm, newdata = testDF, best.iter)
  globalRisk <- exp(testDF.linPred) # global risk
  s.12mon <- exp(-cumulativeHaz[1])^globalRisk # Survival prob at 12 months
  s.18mon <- exp(-cumulativeHaz[2])^globalRisk # Survival prob at 18 months
  s.24mon <- exp(-cumulativeHaz[3])^globalRisk # Survival prob at 24 months
  risksDF <- data.frame(globalRisk, s.12mon, s.18mon, s.24mon)
  return(risksDF)     
}


# coxph regression used to obtain exact time to event
model.coxph.time2event <- function(dataObject){
  coxfit <- coxph(formula=Surv(dataObject$time, dataObject$event) ~ ., data=dataObject$trainingDF, method="efron")
  globalRisk <- predict(coxfit, newdata=dataObject$testDF, type="risk")  # global (relative risk)
  
  # calculates the survival probabilites for each test patient from day 1 through the maximum survival day of the training set
  survivalProb <- predictSurvProb(coxfit, newdata=dataObject$testDF, times=seq(1,max(dataObject$time),1)) 
  
  exactTime2Event <- matrix(data=0,nrow=nrow(dataObject$testDF),ncol=1)
  
  # scans through each patient looking for the first day where survival probability is less or equal to 0.502
  for (i in 1:nrow(dataObject$testDF)){
    firstElementCount <- 0
    for (j in 1:ncol(survivalProb)){
      if (survivalProb[i,j] <= 0.502) {
        firstElementCount <- 1
        exactTime2Event[i] <- j
      }
      
      if (j == ncol(survivalProb)){
        exactTime2Event[i] <- j
      }
      
      if (firstElementCount == 1) {
        break
      }
    }
  }
  
  
  return(exactTime2Event)
}


# gradient boost machine with coxph used to obtain exact time to event
model.gbmcoxph.time2event <- function(dataObject, numTrees, depth, bagFraction, shrink){
  
  time <- dataObject$time
  event <- dataObject$event
  trainingDF <- dataObject$trainingDF
  testDF <- dataObject$testDF
  
  coxphfit.gbm <- gbm(Surv(time, event) ~ ., distribution = "coxph", n.trees = numTrees, data = trainingDF, shrinkage= shrink,
                      interaction.depth = depth, bag.fraction = bagFraction, cv.folds = 5, keep.data = TRUE, verbose = FALSE)
  
  best.iter <- gbm.perf(coxphfit.gbm, method = "cv", plot.it=FALSE)   # returns test set estimate of best number of trees
  cumulativeHaz <- basehaz.gbm(time, event, coxphfit.gbm$fit, t.eval = seq(1,max(dataObject$time),1), cumulative = TRUE)
  testDF.linPred <- predict(coxphfit.gbm, newdata = testDF, best.iter)
  globalRisk <- exp(testDF.linPred) # global risk
  
  exactTime2Event <- matrix(data=0,nrow=nrow(dataObject$testDF),ncol=1)
  survivalProb <- matrix(data=0,nrow=nrow(dataObject$testDF),ncol=max(dataObject$time))
  
  # calculates the survival probabilites for each test patient from day 1 through the maximum survival day of the training set
  for (k in 1:ncol(survivalProb)){
    survivalProb[,k] <- exp(-cumulativeHaz[k])^globalRisk
  }
  survivalProb[is.na(survivalProb)] <- 1

  # scans through each patient looking for the first day where survival probability is less or equal to 0.502
  for (i in 1:nrow(dataObject$testDF)){
    firstElementCount <- 0
    for (j in 1:ncol(survivalProb)){
      if (survivalProb[i,j] <= 0.502) {
        firstElementCount <- 1
        exactTime2Event[i] <- j 
      }
      
      if (j == ncol(survivalProb)){
        exactTime2Event[i] <- j 
      }
      
      if (firstElementCount == 1) {
        break
      }
    }
  }
  
  return(exactTime2Event)    
}


# gradient boost machine with ci used to obtain exact time to event
model.gbmci.time2event <- function(dataObject, numTrees, depth, bagFraction, shrink){
  
  time <- dataObject$time
  event <- dataObject$event
  trainingDF <- dataObject$trainingDF
  testDF <- dataObject$testDF
  
  coxphfit.gbm <- gbm(Surv(time, event) ~ ., distribution = "sci", n.trees = numTrees, data = trainingDF, shrinkage= shrink,
                      interaction.depth = depth, bag.fraction = bagFraction, cv.folds = 5, keep.data = TRUE, verbose = FALSE)
  
  best.iter <- gbm.perf(coxphfit.gbm, method = "cv", plot.it=FALSE)   # returns test set estimate of best number of trees
  cumulativeHaz <- basehaz.gbm(time, event, coxphfit.gbm$fit, t.eval = seq(1,max(dataObject$time),1), cumulative = TRUE)
  testDF.linPred <- predict(coxphfit.gbm, newdata = testDF, best.iter)
  globalRisk <- exp(testDF.linPred) # global risk
  
  exactTime2Event <- matrix(data=0,nrow=nrow(dataObject$testDF),ncol=1)
  survivalProb <- matrix(data=0,nrow=nrow(dataObject$testDF),ncol=max(dataObject$time))
  
  # calculates the survival probabilites for each test patient from day 1 through the maximum survival day of the training set
  for (k in 1:ncol(survivalProb)){
    survivalProb[,k] <- exp(-cumulativeHaz[k])^globalRisk
  }
  survivalProb[is.na(survivalProb)] <- 1

  # scans through each patient looking for the first day where survival probability is less or equal to 0.502
  for (i in 1:nrow(dataObject$testDF)){
    firstElementCount <- 0
    for (j in 1:ncol(survivalProb)){
      if (survivalProb[i,j] <= 0.502) {
        firstElementCount <- 1
        exactTime2Event[i] <- j 
      }
      
      if (j == ncol(survivalProb)){
        exactTime2Event[i] <- j
      }
      
      if (firstElementCount == 1) {
        break
      }
    }
  }
  
  return(exactTime2Event)    
}

