setwd("C:/Users/Pol/Desktop/stacking")
library(caret)
library(pROC)
library(ipred)
library(plyr)
library(e1071)
library(randomForest)
library(fastAdaboost)
library(rpart)
library(party)
library(klaR)
library(kernlab)
library(C50)
library(nnet)
library(mboost)
library(foreach)
library(import)
library(mlbench)
library(caret)
library(caretEnsemble)
library(pROC)

set.seed(2018)
speedData <- read.csv("SP2.csv")
speedData <- speedData[,-c(8,44)]
speedData$match[speedData$match==1]<-"Y"
speedData$match[speedData$match==0]<-"N"
speedData$match <- as.factor(speedData$match)


control <- trainControl(method="cv", number=7, 
                        savePredictions=TRUE, classProbs=TRUE,
                        summaryFunction = twoClassSummary)


##Just adaboost
RF_adaboost <- train(match~.,data=speedData,method="adaboost",
                     trControl=control,metric="ROC",
                     prox=TRUE,allowParallel=TRUE)
print(RF_adaboost)


##Just RF
RF <- train(match~.,data=speedData,method="rf",
                     trControl=control,metric="ROC",
                     prox=TRUE,allowParallel=TRUE)
print(RF_adaboost)

##Just C5.0
c50Grid <- expand.grid(.trials = c(1:9, (1:10)*10),
                       .model = c("tree", "rules"),
                       .winnow = c(TRUE, FALSE))
c5Fitvac <- train(match~ .,
                  data = speedData,
                  method = "C5.0",
                  tuneGrid = c50Grid,
                  trControl = control,
                  metric = "ROC", 
                  importance=TRUE) 
print(c5Fitvac)

###THREE MODELS BEST CORRELATE###
algorithmList <- c('adaboost',"glm","nb","svmRadialCost")
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

adaboosted <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(adaboosted)


algorithmList <- c("pcaNNet","glm")
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

PCAone <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(PCAone)

algorithmList <- c("rf","glm")
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

refindRF <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(refindRF)



lastResult <- resamples(list(refinedAdaboost=adaboosted$ens_model,
                             refinedNN=PCAone$ens_model,
                             refinedRF=refindRF$ens_model,
                             Adaboost=RF_adaboost,
                             C5.0=c5Fitvac,RandomForest=RF))

print(adaboosted$ens_model)
print(PCAone$ens_model)
print(refindRF$ens_model)
print(RF_adaboost)
print(RF)
print(refindRF)
print(c5Fitvac)

bwplot(lastResult,metric="ROC",main="")

###THREE MODELS BEST CORRELATE###







