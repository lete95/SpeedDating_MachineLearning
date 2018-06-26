set.seed(123)
require(readxl)
require(caret)
require(randomForest)
require(RANN)
require(mice)

data<- read.csv("SP.csv",header = TRUE, quote = "\"", dec = ".", check.names=TRUE)


variablesToEliminate <- c("d_importance_same_race",
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
                          "d_guess_prob_liked","decision","decision_o","like")

data <- data[ , !(names(data) %in% variablesToEliminate)]

##Create partitons
inTrain <- createDataPartition(y=data$match, p=0.66, list=FALSE)
data.Training <- data[inTrain, ]
data.Testing <- data[-inTrain, ]

##Fit the Random Forest predictor, with 500 trees due to computing boundaries
res.Forest = randomForest(match~., data=data.Training, ntree = 200)
prediction.Testing = round(predict(res.Forest, newdata = data.Testing))
confusionMatrix(as.factor(prediction.Testing), as.factor(data.Testing$match))
varImpPlot(res.Forest)

##Use the feature selection varImpPlot to and eliminate redundant variables
##SECOND ROUND
variablesToMaintain <- c("attractive_o","attractive_partner","funny_o","funny_partner","shared_interests_correlate",
                         "interests_correlate","shared_interests_o","field","guess_prob_liked","pref_o_attractive",
                         "expected_num_matches","attractive_important","pref_o_intelligence","match")
data_processed <- data[ , (names(data) %in% variablesToMaintain)]
##Create partitons
inTrain <- createDataPartition(y=data_processed$match, p=0.66, list=FALSE)
data.Training <- data_processed[inTrain, ]
data.Testing <- data_processed[-inTrain, ]
##Fit the Random Forest predictor, with 500 trees due to computing boundaries
res.Forest = randomForest(match~., data=data.Training, ntree = 200)
prediction.Testing = round(predict(res.Forest, newdata = data.Testing))
confusionMatrix(as.factor(prediction.Testing), as.factor(data.Testing$match))
varImpPlot(res.Forest)

##Use the feature selection varImpPlot to and eliminate redundant variables
##THIRD ROUND
variablesToMaintain <- c("attractive_o","attractive_partner","funny_o","funny_partner","shared_interests_correlate",
                         "match")
data_processed <- data[ , (names(data) %in% variablesToMaintain)]

##Create partitons
inTrain <- createDataPartition(y=data_processed$match, p=0.66, list=FALSE)
data.Training <- data_processed[inTrain, ]
data.Testing <- data_processed[-inTrain, ]
##Fit the Random Forest predictor, with 500 trees due to computing boundaries
res.Forest = randomForest(match~., data=data.Training, ntree = 200)
prediction.Testing = round(predict(res.Forest, newdata = data.Testing))
confusionMatrix(as.factor(prediction.Testing), as.factor(data.Testing$match))
varImpPlot(res.Forest)

##LAST ROUND JUST WITH ATTRACTIVE

variablesToMaintain <- c("attractive_o","attractive_partner",
                         "match")
data_processed <- data[ , (names(data) %in% variablesToMaintain)]
##Create partitons
inTrain <- createDataPartition(y=data_processed$match, p=0.66, list=FALSE)
data.Training <- data_processed[inTrain, ]
data.Testing <- data_processed[-inTrain, ]
##Fit the Random Forest predictor, with 500 trees due to computing boundaries
res.Forest = randomForest(match~., data=data.Training, ntree = 200)
prediction.Testing = round(predict(res.Forest, newdata = data.Testing))
confusionMatrix(as.factor(prediction.Testing), as.factor(data.Testing$match))
varImpPlot(res.Forest)
##END##
################################################################################