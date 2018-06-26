### Loading packages
require(caret)
library(MASS)
library(lars)
library(e1071)
library(tree)
library(randomForest)


#Function that computes the classification of the Match according to the features selected using several classificators.
#It generates a plot with the error prediction for each classificator.
classificate <- function(match, features, train, test){
  

  ### Common parameters
  match_column_features <- which(names(features)=="match")
  match_column_train <- which(names(train)=="match")
  test_without_match <- test[,!names(test) %in% c("match")]
  N.test <- length(test$match)
  
  
  ##### Generating the formula to apply in the classifiers.
  prev <-data.frame()
  
  feats = c()
  
  first<- TRUE
  for (h in 1:length(features)){
    if(h != match_column_features){
      if(first){
        prev <- colnames(features)[h]
        first<- FALSE
      }
      else{
        prev<- paste( prev, colnames(features)[h], sep=" + ")
      }
      feats<- c(feats, colnames(features)[h])
    }
  }
  fm <- as.formula( paste( colnames(features)[match_column_features], prev, sep=" ~ "))
  
  
  ##### Linear Regression
  
  (model.linreg <- lm(train$match ~ ., data=train)) 
  
  rounded_values= round(data.frame(predict(model.linreg, test_without_match)))
  (pred.linreg <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### RIDGE REGRESSION
  
  model.ridge <- lm.ridge(fm , data=train, lambda = seq(0,10,0.1))
  
  (lambda.ridge <- seq(0,10,0.1)[which.min(model.ridge$GCV)])
  
  model.ridgereg.FINAL <- lm.ridge(fm , data=train, lambda = lambda.ridge)
  
  (beta.ridgereg.FINAL <- coef(model.ridgereg.FINAL))
  
  rounded_values <- round(beta.ridgereg.FINAL[1] + as.matrix(test_without_match[,feats])%*%beta.ridgereg.FINAL[-1])
  
  (pred.ridgereg <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### LASSO REGRESSION
  
  t <- as.numeric(train[,c("match")])
  x <- as.matrix(train[,feats]) #Problems with string values.
  
  model.lasso <- lars(x, t, type="lasso")
  
  rounded_values <- round(predict(model.lasso, as.matrix(test[,feats]), s=2, type="fit")$fit)
  
  (pred.lasso <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### SVM
  
  ## model 1: linear kernel, C=1 (cost parameter)
  (model <- svm(train[,feats],train$match, type="C-classification", cost=1, kernel="linear", scale = FALSE))
  pred_svm1 <- predict(model, train[,feats])
  
  rounded_values= round(data.frame(as.numeric(pred_svm1)))
  (pred.svm1 <- sum((test$match - rounded_values)^2)/N.test)
  
  ## model 2: linear kernel, C=0.1 (cost parameter)
  (model <- svm(train[,feats],train[,match_column_train], type="C-classification", cost=0.1, kernel="linear", scale = FALSE))
  pred_svm2 <- predict(model, train[,feats])
  
  rounded_values= round(data.frame(as.numeric(pred_svm2)))
  (pred.svm2 <- sum((test$match - rounded_values)^2)/N.test)
  
  ## model 3: linear kernel, C=25 (cost parameter)
  (model <- svm(train[,feats],train[,match_column_train], type="C-classification", cost=25, kernel="linear", scale = FALSE))
  pred_svm3 <- predict(model, train[,feats])
  
  rounded_values= round(data.frame(as.numeric(pred_svm3)))
  (pred.svm3 <- sum((test$match - rounded_values)^2)/N.test)
  
  # compute decision values and probabilities:
  pred_svm <- predict(model,train[,feats], decision.values = TRUE)
  rounded_values= round(data.frame(as.numeric(pred_svm)))
  (pred.svm <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### Naïve Bayes classificator:
  
  class <-paste(paste("as.factor(",colnames(features)[match_column_features],sep=""), ")", sep="")
  fm2 <- as.formula( paste( class, prev, sep=" ~ "))
  
  model <- naiveBayes(fm2, data = features)
  pred_naive <- predict(model, features[,-match_column_features])
  
  rounded_values= round(data.frame(as.numeric(pred_naive)))
  (pred.naive <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### Tree
  
  model.tree <- tree (fm, data=train)
  pred_tree <- predict (model.tree, features[,-match_column_features])
  
  rounded_values= round(data.frame(pred_tree))
  (pred.tree <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ##### RandomForestS
  
  model.rf1 <- randomForest(fm, data=train, ntree=100, proximity=FALSE)
  pred_rf1 <- predict (model.rf1, features[,-match_column_features], type="class")
  
  rounded_values= round(data.frame(pred_rf1))
  (pred.rf1 <- sum((test$match - rounded_values)^2)/N.test)
  
  
  ### Generating BARPLOT over the predictions.
  
  predictions <- c(pred.linreg,pred.ridgereg, pred.lasso, pred.svm, pred.svm1, pred.svm2, pred.svm3, pred.naive, pred.tree, pred.rf1)
  prediction_names  <- c("Linear R.", "Ridge R.", "LASSO R.", "SVM", "SVM1", "SVM2", "SVM3", "Naive B.", "Tree", "R.Forest")
  
  prediction_values <- data.frame("Predictions" = predictions)
  par(mai=c(1,1,1,1))
  xx <-barplot(prediction_values$Predictions,main="Prediction errors",las=1)
  axis(1, at=seq(0.75,11.75,by=1.2),labels=prediction_names, las = 2)
  #text(x = xx, y = prediction_values$Predictions, label = prediction_values$Predictions, pos = 3, cex = 0.8, col = "red")
  #text(cex=1, x=x-.25, y=-1.25, prediction_names, xpd=TRUE, srt=45)
  

}

### Loading dataset & initialize some variables.
SpeedDating <- read.csv("speeddating_without_na.csv")
NewSpeedDating <-read.csv("SP.csv")

decision_column <- which(names(NewSpeedDating)=="decision")
NewSpeedDating <- NewSpeedDating[,-decision_column]
decision_o_column <- which(names(NewSpeedDating)=="decision_o")
NewSpeedDating <- NewSpeedDating[,-decision_o_column]

summary(SpeedDating)

#Generating train and test without Cross-Validation
n <- nrow(NewSpeedDating)
train <- NewSpeedDating[1:round(2/3*n),]
test <- NewSpeedDating[(round(2/3*n)+1):n,]

#Cross Validation.
classes <- NewSpeedDating[,"match"]
intrain_set <- createDataPartition(classes, p = 0.8, list = FALSE)

new_train <- NewSpeedDating[intrain_set,]
new_test <- NewSpeedDating[-intrain_set,]



attach(NewSpeedDating)


### Classificate Match over 'age' and 'age_o' features.
data = new_train[,c("match", "age", "age_o")]
classificate(match, data , new_train, new_test)

### Classificate Match over ALL the features.
classificate(match, new_train , new_train, new_test)

### Classificate Match over the best features.
initialModel <- lm(match ~ ., data = new_train)
bestModel <- step(initialModel, direction = "both")

bestFeatures <- names(bestModel$coefficients[-1]) #Removing intercept parameter.
data <- new_train[,c("match",bestFeatures)] #We need to add the match feature.
classificate(match, data , new_train, new_test)


################################################################################################

features <- new_train

train <- new_train
test <- new_test


