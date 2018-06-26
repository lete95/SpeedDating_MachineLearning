library(caret)
library(pROC)
library(ipred)
library(plyr)
library(e1071)

set.seed(2018)

speedData <-read.csv("SP.csv")
speedData <- speedData[,-c(47,48,8,44)]

speedData$match[speedData$match=="1"] <- "Y"
speedData$match[speedData$match=="0"] <- "N"
speedData$match <- as.factor(speedData$match)

control <- trainControl(method="cv", number=3, classProbs=TRUE, summaryFunction=twoClassSummary)

##TRAIN THE CLASSIFICATION METHODS##
##RandomForest##
require(randomForest)
RF <- train(match~.,data=speedData,method="rf",
            trControl=control,metric="ROC",
            prox=TRUE,allowParallel=TRUE)
print(RF)
##RandomForest 2##
require(fastAdaboost)
RF_adaboost <- train(match~.,data=speedData,method="adaboost",
            trControl=control,metric="ROC",
            prox=TRUE,allowParallel=TRUE)
print(RF_adaboost)
##RandomForest 3##
require(rpart)
rpart_RF <- train(match~.,data=speedData,method="rpart",
                     trControl=control,metric="ROC")
print(rpart_RF)
##RandomForest 4##
require(party)
rpart_RF <- train(match~.,data=speedData,method="cforest",
                  trControl=control,metric="ROC")
print(rpart_RF)
##Naive Bayes##
library(klaR)
grid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))
NB <- train(match~.,data=speedData,method="nb",tuneGrid=grid,
            trControl=control,metric="ROC")
print(NB)
##GLM
NB <- train(match~.,data=speedData,method="glm",
            trControl=control,metric="ROC")
print(GLM)
##SVM's##
library(kernlab)
library(e1071)

grid <- data.frame(C=c(0.1,0.25,0.5,0.75,1.0))
SVM1 <- train(match~.,data=speedData,method="svmLinear",tuneGrid=grid,
            trControl=control,metric="ROC")
print(SVM1)

grid <- data.frame(C=c(0.1,0.25,0.5,0.75,1.0))
SVM_Radial <- train(match~.,data=speedData,method="svmRadialCost",tuneGrid=grid,
              trControl=control,metric="ROC")
print(SVM_Radial)
##Just a simple tree
CART <- train(match~.,data=speedData,method="rpart",
                    trControl=control,metric="ROC")
print(CART)
##Neural Networks
ANN <- train(match~.,data=speedData,method="nnet",tuneGrid=expand.grid(size=c(10), decay=c(0.1)),
              trControl=control,metric="ROC",maxit=1000)
print(ANN)

PCAANN <- train(match~.,data=speedData,method="pcaNNet",tuneGrid=expand.grid(size=c(10,20), decay=c(0.1,0.5)),
             trControl=control,metric="ROC",maxit=1000)
print(PCAANN)

avNNet <- train(match~.,data=speedData,method="avNNet",tuneGrid=expand.grid(size=c(10,20), decay=c(0.1,0.5),bag=TRUE),
                trControl=control,metric="ROC",maxit=1000)
print(avNNet)

##Get the output of each method and compare

rValues <- resamples(list(ANN=ANN,averagedNNet=avNNet,PCAANN=PCAANN))
bwplot(rValues,metric="ROC",main="Artificial Neural Network Methods")

rValues2 <- resamples(list(RandomForest=RF,DecTRee=rpart_RF,RFAdaboost=RF_adaboost))
bwplot(rValues2,metric="ROC",main="Random Forest Based Methods")

rValues3 <- resamples(list(SVMLinear=SVMLinear1,SVMRadial=SVM_Radial))
bwplot(rValues3,metric="ROC",main="Support Vector Machine Methods")

rALLMETHODS <- resamples(list(SVMRadial=SVM_Radial,RFAdaboost=RF_adaboost,PCAANN=PCAANN,DecisionTree=CART))
bwplot(rALLMETHODS,metric="ROC",main="All Classification Algorithms Compared")

modelCor(rALLMETHODS)
splom(rALLMETHODS)
##With the package caretEnsamble, we will try to ensamble the models with better outcome


