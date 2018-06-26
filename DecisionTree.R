##RANDOM FOREST FOR ML PROJECT SPEEDDATING

require("dplyr")        # Data manipulation
require("reshape2")     # Data reshaping for ggplot
require("ggplot2")      # Data visualization
require("plotly")       # Dynamic data visualization
require("RColorBrewer") # Colors on plots
require("readr")        # CSV file I/O, e.g. the read_csv function
require("dataQualityR") # DQR generation
require("randomForest") # Random Forest for variable importance
require("scales")       # Colour palette
require("fmsb")         # Radar plots
require("rpart")        # Install all libraries if requires
set.seed(1234)
##DATA READING



data<- read.csv("SP.csv",header = TRUE, quote = "\"", dec = ".", check.names=TRUE)


variablesToEliminate <- c("has_null","d_importance_same_race",
                          "d_importance_same_religion","d_pref_o_attractive",
                          "d_pref_o_sincere","d_pref_o_intelligence","d_pref_o_funny","d_pref_o_ambitious",
                          "d_pref_o_shared_interests","d_attractive_o","d_sinsere_o","d_intelligence_o",
                          "d_funny_o","d_ambitous_o","d_shared_interests_o",
                          "d_attractive_important","d_sincere_important","d_intellicence_important","d_funny_important",
                          "d_ambtition_important","d_shared_interests_important","d_attractive","d_sincere","d_intelligence","d_funny","d_ambition",
                          "d_sincere_partner","d_d_age",
                          "d_intelligence_partner","d_funny_partner","d_ambition_partner","d_shared_interests_partner",
                          "sports","tvsports","exercise","dining","museums","art","hiking","gaming","clubbing","reading",
                          "tv","theater","movies","concerts","music","shopping","yoga","d_sports","d_tvsports",
                          "d_exercise","d_dining","d_museums","d_art","d_hiking","d_gaming","d_clubbing","d_reading",
                          "d_tv","d_theater","d_movies","d_concerts","d_music","d_shopping","d_yoga",
                          "d_interests_correlate","d_attractive_partner",
                          "d_expected_happy_with_sd_people",
                          "d_expected_num_interested_in_me","d_expected_num_matches","d_like",
                          "d_guess_prob_liked","decision","decision_o")
data <- data[ , !(names(data) %in% variablesToEliminate)]
dim(data)
##DATA READING


##SELECTING TRAINING AND TEST SETS
boundary <- round(nrow(data)*2/3)
train_data <- data[1:boundary,]
test_data <- data[-(1:boundary),]
##SELECTING TRAINING AND TEST SETS

##FIRST DECISION TREE TO PREDICT THE MATCH VARIABLE
library(rpart)
p2 <- rpart(match ~ ., data=train_data,control=rpart.control(cp=0.001, xval=10))
printcp(p2)
plotcp(p2)
p2$cptable <- as.data.frame(p2$cptable)
ind <- which.min(p2$cptable$xerror)
#   Minimum xerror found 
xerr <- p2$cptable$xerror[ind]
xstd <- p2$cptable$xstd[ind]
i = which(p2$cptable$xerror < xerr+xstd)[1]
#   Cutoff value
alfa = p2$cptable$CP[i]
plot(p2$cptable[,2],p2$cptable[,3],type="l", xlab = "Size of the tree", ylab = "Relative impurity")
lines(p2$cptable[,2],p2$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)
abline(v=i, col="red", lty = 2)
#   Prune the tree
p1 <- prune(p2,cp=alfa)
library(rpart.plot)
rpart.plot(p1)
##FIRST DECISION TREE TO PREDICT THE MATCH VARIABLE

#This decision tree seems to be too dependant on the "like" variable, which gives a lot of information about the match.
#However, lets see what happens if we eliminate this variable, as we consider that to get a deeper knowledge about
#what makes participants to have match or not in other aspects.

##SECOND DECISION TREE TO PREDICT THE MATCH VARIABLE
variablesToEliminate <- c("like")
data <- data[ , !(names(data) %in% variablesToEliminate)]
dim(data)
##SELECTING TRAINING AND TEST SETS
boundary <- round(nrow(data)*2/3)
train_data <- data[1:boundary,]
test_data <- data[-(1:boundary),]
##SELECTING TRAINING AND TEST SETS

##FIRST DECISION TREE TO PREDICT THE MATCH VARIABLE
library(rpart)
p2 <- rpart(match ~ ., data=train_data,control=rpart.control(cp=0.001, xval=10),method="class")
printcp(p2)
plotcp(p2)
p2$cptable <- as.data.frame(p2$cptable)
ind <- which.min(p2$cptable$xerror)
#   Minimum xerror found in size 3
xerr <- p2$cptable$xerror[ind]
xstd <- p2$cptable$xstd[ind]
i = which(p2$cptable$xerror < xerr+xstd)[1]
#   Cutoff value is 3
alfa = p2$cptable$CP[i]
alfa=p2$cptable$CP[4]
plot(p2$cptable[,2],p2$cptable[,3],type="l", xlab = "Size of the tree", ylab = "Relative impurity")
lines(p2$cptable[,2],p2$cptable[,4],col="blue")
legend("topright",c("R(T)training","R(T)cv"),col=c("black","blue"),lty=1)
abline(v=i, col="red", lty = 2)
#   Prune the tree
p2 <- prune(p2,cp=alfa)
library(rpart.plot)
rpart.plot(p2,varlen=25)
##SECOND DECISION TREE TO PREDICT THE MATCH VARIABLE

##PLOT IMPORTANCE OF VARIABLES
p2$variable.importance
barplot(p2$variable.importance[1:5],las=TRUE,cex.names=0.65,
        main = "Variable Importance without like",col=ifelse(p2$variable.importance[1:7]>30,"orange","#ffc080"))

barplot(p1$variable.importance[1:5],las=TRUE,cex.names=0.65,
        main = "Variable Importance with like",col=ifelse(p2$variable.importance[1:7]>30,"orange","#ffc080"))

##PLOT IMPORTANCE OF VARIABLES

##GET PERFORMANCE INDICATORS
prediction <- predict(p2, newdata=test_data,type = "class")

ConfusionMatrix <- function(pred, truth) {
    cm <- table(Truth = truth,Pred = pred)
    print(cm)
    #   Accuracy 
    accuracy <- sum(diag(cm))/sum(cm)
    print(paste0("Accuracy: ", round(accuracy*100,2),"%" ))
    
    #   Precision
    precision.p <- cm[1,1]/sum(cm[,1])
    precision.n <- cm[2,2]/sum(cm[,2])
    precision.mean <- (precision.p + precision.n)/2
    print(paste0("Positive Prediction: ", round(precision.p*100,2),"%" ))
    print(paste0("Negative Prediction: ", round(precision.n*100,2),"%" ))
    print(paste0("Mean Prediction: ", round(precision.mean*100,2),"%" ))
    
    #   Recall
    recall <- cm[1,1]/sum(cm[1,])
    print(paste0("Recall: ", round(recall*100,2),"%" ))
    
    #   Auc
    library(ROCR)
    pred <- prediction(as.numeric(levels(pred))[pred],truth)
    roc <- performance(pred,measure = "tpr",x.measure = "fpr")
    plot(roc, main="ROC Curve")
    abline(0,1,col="blue")
    auc = performance(pred,"auc")
    auc = as.numeric(auc@y.values)
    print(paste0("AUC: ", round(auc*100,2),"%" ))
}

ConfusionMatrix(prediction, test_data$match)
