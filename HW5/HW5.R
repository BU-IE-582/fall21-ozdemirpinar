#---
title: "Homework_5"
author: "Pýnar Özdemir"
date: "27/01/2022"
#---

# Required libraries
library(tidyr)
library(openxlsx)
library(xlsx)
library(data.table)
library(Matrix)
library(tidyverse)
library(dplyr)
library(glmnet)
library(caret)
library(rpart)
library(randomForest)
library(e1071)
library(ranger)
library(gbm)
library(pROC)
library(skimr)
library(corrplot)
library(TunePareto)
library(ggplot2)
library(fastmatch)
library(smotefamily)
library(cluster)
library(factoextra)


# Reading and showing data
train_dataset <- read.csv("C:/Users/Pinar/Downloads/train.csv", header = T)
#head
head(train_dataset)
#skim (1715 rows and 63 columns)
skim(train_dataset)
#glimpse
glimpse(train_dataset)
# dimension
dim(train_dataset)

# Checking for missing value 
train_dataset[train_dataset  == ""] <- NA
train_dataset [train_dataset  == " "] <- NA

# counting for NA
listofna <- cbind(lapply(lapply(train_dataset, is.na), sum))
listofna # there are any NA

# looking the train data set
str(train_dataset)

# changing Var_39 and Var_53 features to numeric
#for Var_39
unique(train_dataset$Var_39)
train_dataset$Var_39=ifelse(train_dataset$Var_39== "Y",1,0)
#for Var_53
unique(train_dataset$Var_53)
train_dataset$Var_53=ifelse(train_dataset$Var_53== "Y",1,0)

# Class distribution of train data set and correlation matrix 
table(train_dataset$default)
ggplot(train_dataset, aes(x=default)) + geom_bar(color="blue", fill = "white")
# check correlation
# creating correlation matrix without loan_application_id
correlation_matrix = cor(train_dataset[,-1])
corrplot(correlation_matrix, method = "color")

# omitting loan_application_id from train data set
train_dataset =  train_dataset [,-1]
head(train_dataset)

# Creating new train and test part from train_dataset with 0.7 and 0.3 ratios
set.seed(1)
index <- createDataPartition(train_dataset$default, p=0.7, list=FALSE)
trainn <- train_dataset[ index,]
testt <- train_dataset[-index,]
sum(trainn$default)
# 115 
length(trainn$default)  
# 1201
length(testt$default) 
# 514
sum(testt$default) 
# 57

# Class distribution of new train data set  
table(trainn$default)
ggplot(trainn, aes(x=default)) + geom_bar(color="blue", fill = "purple")
# 1086, 115

head(trainn)

# scaling except default for train and test subset data
# for train subset
scaled_train <- as.data.table(scale(trainn[,-c(1,2)]))
# addition of default
scaled_trainn <- cbind(scaled_train, trainn$default)
colnames(scaled_trainn)[61] <-  "default" 

# for test subset
scaled_test <- as.data.table(scale(testt[,-c(1,2)]))
# addition of default
scaled_testt <- cbind(scaled_test, testt$default)
colnames(scaled_testt)[61] <-  "default" 

### Calculation for a custom performance metric
# for pred=1, obs=1 - 0
# for pred=1, obs=0 - (0,15)x(loan_amount)
# for pred=0, obs=1 - (loan_amount)
# for pred=0, obs=0 - 0

### Creating function for custom performance metric of total money lost for train data

totalcost_func = function(data, lev = NULL , model= NULL ){
data$pred <- as.numeric(data$pred)-1
data$obs <- as.numeric(data$obs)-1 
listofloan <- trainn$loan_amount[data$rowIndex]
cost= 0
for (a in 1:length(data$obs)){
if  ( data$obs [a] == 1  & data$pred[a] == 0  ){
cost <- cost+listofloan [a]}
if  ( data$obs [a] == 0  & data$pred[a] == 1 ){
cost = cost+listofloan [a] * (0.15)}}
names(cost) <- c('total money lost')
cost
}

### Creating the function to observe performance of test data
cost_test <- function(pred, true ){
te_co <- 0
for (b in 1:length(pred)){
if  (true [b] == 1 & pred[b] == 0){
te_co <- te_co+testt$loan_amount [b]}
if  (true [b] == 0 & pred[b] == 1){
te_co <- te_co+testt$loan_amount [b] * (0.15)}} 
names(te_co) <- c('total money lost_test')
te_co
}

# In modeling part Stochastic Gradient Boosting (SGB) approach is adopted for the classification goal.
#model was trained with scaled_trainn data set and tested on scaled_testt

# to avoid class imbalance problem, up, down and smote sampling techniques were used.

# Model1: SGB - up sampling

set.seed(1)
train_cont_up <- trainControl(summaryFunction = totalcost_func, method="cv", number=10, savePredictions=TRUE, sampling="up")
up_grid=expand.grid(interaction.depth = c(3,5,7), n.trees = c(300,500,700), shrinkage = c(0.01),n.minobsinnode = c(3,5,7))
up_train <- train(as.factor(default) ~., data = scaled_trainn,
                  method = "gbm",  metric = "total money lost" ,
                  tuneGrid = up_grid ,
                  trControl = train_cont_up)

up_train
up_train$finalModel

pred_up <- predict(up_train, scaled_testt[,-61])
table(pred_up,  as.factor(scaled_testt$default))

con_mat_up <- confusionMatrix(pred_up , as.factor(testt$default))
con_mat_up

te_co_up <- cost_test(pred_up,  scaled_testt$default)
te_co_up

# Model2: SGB - down sampling

set.seed(1)
train_cont_down <- trainControl(summaryFunction = totalcost_func, method="cv", number=10, savePredictions=TRUE, sampling="down")
down_grid <- expand.grid(interaction.depth = c(3,5,7), n.trees = c(300,500,700), shrinkage = c(0.01),n.minobsinnode = c(3,5,7))
down_train <- train(as.factor(default) ~., data = scaled_trainn,
                  method = "gbm",  metric = "total money lost" ,
                  tuneGrid = down_grid ,
                  trControl = train_cont_down) 

pred_down <- predict(down_train, scaled_testt[,-61])
table(pred_down,  as.factor(scaled_testt$default))

con_mat_down <- confusionMatrix(pred_down , as.factor(testt$default))
con_mat_down

te_co_down <- cost_test(pred_down,  scaled_testt$default)
te_co_down

# Model3: SGB - smote sampling

set.seed(1)
train_cont_smote <- trainControl(summaryFunction = totalcost_func, method="cv", number=10, savePredictions=TRUE, sampling="smote")
smote_grid <- expand.grid(interaction.depth = c(3,5,7), n.trees = c(300,500,700), shrinkage = c(0.01),n.minobsinnode = c(3,5,7))
smote_train <- train(as.factor(default) ~., data = scaled_trainn,
                  method = "gbm",  metric = "total money lost" ,
                  tuneGrid = smote_grid ,
                  trControl = train_cont_smote) 

pred_smote <- predict(smote_train, scaled_testt[,-61])
table(pred_smote,  as.factor(scaled_testt$default))

con_mat_smote <- confusionMatrix(pred_smote , as.factor(testt$default))
con_mat_smote

te_co_smote <- cost_test(pred_smote,  scaled_testt$default)
te_co_smote

### Comparison for up, down and smote sampling method on SGB model

model_comparison = resamples(list(SGB_up_sampling = up_train,
                                  SGB_down_sampling = down_train,
                                  SGB_smote_sampling = smote_train))

# values and summary for models
model_comparison$values
summary(model_comparison)

# representation of each model total money losts
bwplot(model_comparison , horizontal = T)

### after comparison Model1: SGB - up sampling was selected due to giving minimum total money lost
# this result also shows suitability of up-sampling on such our data.
# because up-sampling technique replicates the observations from minority class to balance the data.

### Calling real test data and applying same steps as in train
test_dataset <- read.csv("C:/Users/Pinar/Downloads/train.csv", header = T)
#head
head(test_dataset)
#skim 
skim(test_dataset)
#glimpse
glimpse(test_dataset)
# dimension
dim(test_dataset)
# 1715 rows, 63 columns

# changing Var_39 and Var_53 features to numeric
#for Var_39
unique(test_dataset$Var_39)
test_dataset$Var_39=ifelse(test_dataset$Var_39== "Y",1,0)
#for Var_53
unique(test_dataset$Var_53)
test_dataset$Var_53=ifelse(test_dataset$Var_53== "Y",1,0)

# scaling except default for real test data
scaled_testtt <- as.data.table(scale(test_dataset[,-c(1,3)]))
# dimension
dim(scaled_testtt)
# 1715 rows, 61 columns

# Utilization of selected model (Model1:SGB-up sampling) on whole train data set

# scaling except default and loan columns
sca_train <- as.data.table(scale(train_dataset[,-c(1,2)]))
# addition of default
w_scaled_train = cbind(sca_train, train_dataset$default)
colnames(w_scaled_train)[60] <-  "default" 

# Model1: SGB - up sampling FOR WHOLE TRAIN DATA SET

# the best tuning parameter for model1
#n.trees= 700
#interaction.depth = 5
#shrinkage = 0.01
#n.minobsinnode = 7

set.seed(1)
w_train_cont_up <- trainControl(summaryFunction = totalcost_func, method="cv", number=10, savePredictions=TRUE, sampling="up")
w_up_train <- train(as.factor(default) ~., data = w_scaled_train,
                  method = "gbm",  metric = "total money lost" ,
                  tuneGrid = expand.grid(interaction.depth = c(5), n.trees = c(700), shrinkage = c(0.01),n.minobsinnode = c(7)),
                  trControl = w_train_cont_up)

# final model outputs
w_up_train
w_up_train$finalModel

# predictions for final model
w_pred_up <- predict(w_up_train, scaled_testtt)
length(w_pred_up)

# arranging final results
final_result =cbind(test_dataset$loan_application_id, w_pred_up)
final_resultt = cbind(test_dataset$loan_application_id, as.numeric(w_pred_up)-1)
colnames(final_resultt) = c("loan_application_id", "prediction")
# saving results as csv file
write.csv(final_resultt, file="final results for test data set", row.names = FALSE)
