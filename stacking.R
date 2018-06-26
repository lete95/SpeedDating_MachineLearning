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
                        savePredictions=TRUE, classProbs=TRUE,summaryFunction = twoClassSummary)

## SOME MODELS
##MODEL 1
algorithmList <- c('rpart', 'glm', 'knn', 'svmRadial')
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

stack.1 <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(stack.1)

stack.2 <- caretStack(models, method="rf", metric="ROC", trControl=control)
print(stack.2)
##MODEL2
algorithmList <- c('pcaNNet', 'svmRadial')
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList
                    ,tuneGrid=expand.grid(size=c(10,20),decay=c(0.1,0.5)),maxit=500)

stack.3 <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(stack.rf)

stack.4 <- caretStack(models, method="rf", metric="ROC", trControl=control)
print(stack.rf)

##MODEL3
algorithmList <- c('pcaNNet', 'adaboost')
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList
                    ,tuneGrid=expand.grid(size=c(10,20),decay=c(0.1,0.5)),maxit=500
                    ,prox=TRUE,allowParallel=TRUE)

stack.5 <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(stack.rf)

stack.6 <- caretStack(models, method="rf", metric="ROC", trControl=control)
print(stack.rf)

##MODEL5
algorithmList <- c('adaboost',"glm","knn")
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

stack.9 <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(stack.9)

stack.10 <- caretStack(models, method="rf", metric="ROC", trControl=control)
print(stack.10)

##MODEL6
algorithmList <- c("C5.0","glm","knn")
models <- caretList(match~., data=speedData, trControl=control, methodList=algorithmList)

stack.11 <- caretStack(models, method="glm", metric="ROC", trControl=control)
print(stack.11)

stack.12 <- caretStack(models, method="rf", metric="ROC", trControl=control)
print(stack.12)

##Just adaboost
RF_adaboost <- train(match~.,data=speedData,method="adaboost",
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

#COMPARISON

rALLMETHODS <- resamples(list(glmANDknnANDsvmRadialSTACKEDglm=stack.1$ens_model,
                              glmANDknnANDsvmRadialSTACKEDrf=stack.2$ens_model,
                              pcaNNetANDsvmRadialSTACKEDglm=stack.3$ens_model,
                              pcaNNetANDsvmRadialSTACKEDrf=stack.4$ens_model,
                              pcaNNetANDadaboostSTACKEDglm=stack.5$ens_model,
                              pcaNNetANDadaboostSTACKEDrf=stack.6$ens_model,
                              adaboostANDglmANDknnSTACKEDglm=stack.9$ens_model,
                              adaboostANDglmANDknnSTACKEDrf=stack.10$ens_model,
                              C5.0ANDglmANDknnSTACKEDglm=stack.11$ens_model,
                              C5.0ANDglmANDknnSTACKEDrf=stack.12$ens_model,
                              C5.0ANDglmANDknnSTACKEDglmTUNED=stack.13$ens_model,
                              adaboost=RF_adaboost,
                              C5.0=c5Fitvac))
bwplot(rALLMETHODS,metric="ROC",main="")

modelCor(rALLMETHODS)
splom(rALLMETHODS)

## 3 BEST CORRELATE
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
print(refinedRF)

lastResult <- resamples(list(refinedAdaboost=stack.1$ens_model,
                              refinedRF=stack.2$ens_model,
                              refinedNN=stack.3$ens_model))
bwplot(rALLMETHODS,metric="ROC",main="")

modelCor(rALLMETHODS)
splom(rALLMETHODS)







