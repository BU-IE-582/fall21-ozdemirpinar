# IE 582 - Project Codes
# Group 10
# Pýnar Özdemir

### Required Libraries
library(tidyr)
library(openxlsx)
library(xlsx)
library(DMwR2)
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
library(ROSE)
library(pROC)
library(skimr)
library(corrplot)
library(TunePareto)
library(ggplot2)
library(fastmatch)
library(smotefamily)
library(imbalance)

### Reading and showing data set

# for train data
dataset <- fread(file = "IE582_Fall21_ProjectTrain.csv") # train data
glimpse(dataset)
skim(dataset)
dim(dataset)
table(dataset$gender)

# for test data
project_test <- read.csv("IE582_Fall21_ProjectTest.csv") # test data
glimpse (project_test)
skim(project_test)
dim(project_test)

## Rearrangement on train data for obtaining suitable train data set

# keeping unique rows. time stamp and unique id  
dataset <- distinct(dataset, time_stamp,unique_id, .keep_all= TRUE)
colnames(dataset)

# omitted columns
dataset[ ,c("businessunit","product_name","brand_name","Level1_Category_Name","Level2_Category_Name",
            "Level3_Category_Name","type") := NULL]

# turning all blank values into NA values
time_stamp <- dataset$time_stamp
dataset <- dataset[,-1] %>% mutate_all(na_if,"")
dataset$time_stamp <- time_stamp
rm(time_stamp)

# NA values for all columns
dataset[, lapply(.SD, function(x) sum(is.na(x)))]

# if selling price is NA, it is turned into 0
dataset[, sellingprice := ifelse(is.na(sellingprice), 0, sellingprice)]

# checking classes of each column and changing on them
str(dataset)
cols <- c("contentid","brand_id","category_id","Level1_Category_Id","Level2_Category_Id","Level3_Category_Id",
          "user_action","businessunit","product_gender","gender")

# making gender as binary values. "Female" is 1, other is 0.
dataset[, gender := ifelse(gender == "F", 1, 0)]

# sorting dataset by unique_id
dataset <- dataset[order(unique_id)]
gender <- aggregate(dataset, list(dataset$unique_id), FUN=head, 1)
gender <- gender$gender

# creating final table for train data set
dataset_ult <- data.frame(unique_id=numeric(5618),num_of_login=numeric(5618),num_of_basket=numeric(5618),num_of_fav=numeric(5618),
                          num_of_ord=numeric(5618),num_of_search=numeric(5618),num_of_visit=numeric(5618),
                          num_of_gender_woman=numeric(5618),num_of_gender_man=numeric(5618),num_of_gender_unisex=numeric(5618))

dataset_ult <- as.data.table(dataset_ult)
dataset_ult$unique_id <- sort(unique(dataset$unique_id))

# number of visit, search, favorite, basket, order by user
user_action <- dataset %>% count(unique_id, user_action, sort = TRUE)
user_action <- user_action[order(unique_id,user_action)]

bool1 <- user_action$user_action == "basket"
basket <- user_action[bool1,]

for(i in 1:nrow(basket)){
  bask_id = basket$unique_id[i]
  index = which(bask_id == dataset_ult$unique_id)
  dataset_ult[index,3] = basket$n[i]
}

bool2 <- user_action$user_action == "favorite"
fav <- user_action[bool2,] 

for(i in 1:nrow(fav)){
  fav_id = fav$unique_id[i]
  index = which(fav_id == dataset_ult$unique_id)
  dataset_ult[index,4] = fav$n[i]
}

bool3 <- user_action$user_action == "order"
order <- user_action[bool3,]

for(i in 1:nrow(order)){
  order_id = order$unique_id[i]
  index = which(order_id == dataset_ult$unique_id)
  dataset_ult[index,5] = order$n[i]
}

bool4 <- user_action$user_action == "search"
search <- user_action[bool4,]

for(i in 1:nrow(search)){
  search_id = search$unique_id[i]
  index = which(search_id == dataset_ult$unique_id)
  dataset_ult[index,6] = search$n[i]
}

bool5 <- user_action$user_action == "visit"
visit <- user_action[bool5,] 

for(i in 1:nrow(visit)){
  visit_id = visit$unique_id[i]
  index = which(visit_id == dataset_ult$unique_id)
  dataset_ult[index,7] = visit$n[i]
}

# number of log ins by user
total_log  <- dataset %>% count(unique_id, sort = TRUE)
total_log <- total_log[order(unique_id)]
dataset_ult$num_of_login <- total_log$n

# number of log ins by user in work hours (8am-18pm)
dataset$is_inworkhour <- ifelse(hour(dataset$time_stamp)<=18 & hour(dataset$time_stamp)>=8,1,0)  
total_log_workhour <- dataset[, sum(is_inworkhour), by = unique_id]
total_log_workhour <- total_log_workhour[order(unique_id)]
dataset_ult$num_of_login_workhour <- total_log_workhour$V1

total_log_other_hour <- total_log$n-total_log_workhour$V1
dataset_ult$num_of_login_otherhour <- total_log_other_hour

# number of log ins by user at weekdays
days <- weekdays(dataset$time_stamp, abbreviate = TRUE)
dataset$is_weekday <- ifelse(days == "Sat",0,ifelse(days == "Sun",0,1))
total_log_week <- dataset[, sum(is_weekday), by = unique_id]
total_log_week <- total_log_week[order(unique_id)]
dataset_ult$num_of_login_weekday <- total_log_week$V1

total_log_weeknd <- total_log$n-total_log_week$V1
dataset_ult$num_of_login_weekend <- total_log_weeknd

# when user login more at weekdays
is_weekday_log_more <- ifelse(total_log_week$V1 > total_log_weeknd,1,0)
dataset_ult$is_weekday_log_more <- is_weekday_log_more

# when user login more during work hour 
is_workhour_log_more <- ifelse(total_log_workhour$V1 > total_log_other_hour,1,0)
dataset_ult$is_workhour_log_more <- is_workhour_log_more

# total selling price by user
sell_price_total <- dataset[, sum(sellingprice), by = unique_id]
sell_price_total <- sell_price_total[order(unique_id)]
dataset_ult$sell_price_total <- sell_price_total$V1

# average selling price by user
sell_price_ave <- sell_price_total$V1/total_log$n
dataset_ult$sell_price_ave <- sell_price_ave

# is the average selling price of visited products by user over the average? 
is_more_than_avg_selling <- ifelse(sell_price_ave > (sum(sell_price_ave)/5618),1,0)
dataset_ult$is_more_than_avg_selling <- is_more_than_avg_selling

# mostly visited brand id by user
#most_visited_brand <- dataset %>% count(unique_id, brand_id, sort = TRUE)
#most_visited_brand <- most_visited_brand[order(unique_id,-n)]
#most_visited_brand <- aggregate(most_visited_brand, list(most_visited_brand$unique_id), FUN=head, 1)
#most_visited_brand <- most_visited_brand$brand_id
#most_visited_brand  <- ifelse(is.na(most_visited_brand),"Bilinmiyor",most_visited_brand)
#dataset_ult$most_visited_brand_id <- most_visited_brand

# number of visited unique brand id by user
visited_unique_brand_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(brand_id))
dataset_ult$visited_unique_brand_count <- visited_unique_brand_count$`n_distinct(brand_id)`

# is the most visited product gender by user? "female", "male" or "unisex"?
most_visited_gender <- dataset %>% count(unique_id, product_gender, sort = TRUE)
most_visited_gender <- most_visited_gender[order(unique_id,-n)]
most_visited_gender <- aggregate(most_visited_gender, list(most_visited_gender$unique_id), FUN=head, 1)
most_visited_gender <- most_visited_gender$product_gender
most_visited_gender <- ifelse(is.na(most_visited_gender),"Unisex",most_visited_gender)

dataset_ult$most_visited_gender_woman <- ifelse(most_visited_gender == "KadÄ±n",1,0)
dataset_ult$most_visited_gender_man <- ifelse(most_visited_gender == "Erkek",1,0)                                
dataset_ult$most_visited_gender_unisex <- ifelse(most_visited_gender == "Unisex",1,0) 

# number of visit for product gender by user
gender_action <- dataset %>% count(unique_id, product_gender, sort = TRUE)
gender_action <- gender_action[order(unique_id,gender_action)]

bool1 <- gender_action$product_gender == "KadÄ±n"
kadin <- gender_action[bool1,]

for(i in 1:nrow(kadin)){
  kad_id = kadin$unique_id[i]
  index = which(kad_id == dataset_ult$unique_id)
  dataset_ult[index,8] = kadin$n[i]
}

bool2 <- gender_action$product_gender == "Erkek"
erkek <- gender_action[bool2,]

for(i in 1:nrow(erkek)){
  erk_id = erkek$unique_id[i]
  index = which(erk_id == dataset_ult$unique_id)
  dataset_ult[index,9] = erkek$n[i]
}

bool3 <- gender_action$product_gender == "Unisex"
unisex <- gender_action[bool3,]

for(i in 1:nrow(unisex)){
  uni_id = unisex$unique_id[i]
  index = which(uni_id == dataset_ult$unique_id)
  dataset_ult[index,10] = unisex$n[i]
}

# the most visited level 1 category by user
#most_visited_level1 <- dataset %>% count(unique_id, Level1_Category_Id, sort = TRUE)
#most_visited_level1 <- most_visited_level1[order(unique_id,-n)]
#most_visited_level1 <- aggregate(most_visited_level1, list(most_visited_level1$unique_id), FUN=head, 1)
#most_visited_level1 <- most_visited_level1$Level1_Category_Id
#most_visited_level1  <- ifelse(is.na(most_visited_level1),"Bilinmiyor",most_visited_level1)
#dataset_ult$most_visited_level1_id <- most_visited_level1

# number of visited unique category 1 by user
visited_unique_cat1_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level1_Category_Id))
dataset_ult$visited_unique_cat1_count <- visited_unique_cat1_count$`n_distinct(Level1_Category_Id)`

# the most visited level 2 category by user
#most_visited_level2 <- dataset %>% count(unique_id, Level2_Category_Id, sort = TRUE)
#most_visited_level2 <- most_visited_level2[order(unique_id,-n)]
#most_visited_level2 <- aggregate(most_visited_level2, list(most_visited_level2$unique_id), FUN=head, 1)
#most_visited_level2 <- most_visited_level2$Level2_Category_Id
#most_visited_level2  <- ifelse(is.na(most_visited_level2),"Bilinmiyor",most_visited_level2)
#dataset_ult$most_visited_level2_id <- most_visited_level2

# number of visited unique category 2 by user
visited_unique_cat2_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level2_Category_Id))
dataset_ult$visited_unique_cat2_count <- visited_unique_cat2_count$`n_distinct(Level2_Category_Id)`

# the most visited level 3 category by user
#most_visited_level3 <- dataset %>% count(unique_id, Level3_Category_Id, sort = TRUE)
#most_visited_level3 <- most_visited_level3[order(unique_id,-n)]
#most_visited_level3 <- aggregate(most_visited_level3, list(most_visited_level3$unique_id), FUN=head, 1)
#most_visited_level3 <- most_visited_level3$Level3_Category_Id
#most_visited_level3  <- ifelse(is.na(most_visited_level3),"Bilinmiyor",most_visited_level3)
#dataset_ult$most_visited_level3_id <- most_visited_level3

# number of visited unique category 3 by user
visited_unique_cat3_count <- dataset %>% group_by(unique_id) %>% summarise(n_distinct(Level3_Category_Id))
dataset_ult$visited_unique_cat3_count <- visited_unique_cat3_count$`n_distinct(Level3_Category_Id)`

### final suitable train data
dataset_ult$gender <- gender
gend_ind <- which(colnames(dataset_ult) == "gender")
dataset_ult <- dataset_ult[,-1]

# creating xlsx file for final train data table
write.xlsx(dataset_ult, file = "dataset_ult.xlsx", sheetName = "dataset_ult", append = FALSE)

# [Until now, same steps were applied for test data set and
# obtained a suitable data table for test data like train in called "test_için_veri" in the excel form.]

# calling suitable test data table for prediction
project_test_ult <- read.xlsx(file.choose(), 1) # selected "test_için_veri" xlsx file.

# checking data in terms of gender distribution
table(dataset_ult$gender)
ggplot(dataset_ult, aes(x=gender)) + geom_bar(color="blue", fill = "white")

### Modeling part for dataset ##################################################

## Model1 - Decision Trees with weighted resampling ############################

set.seed(1)

model_weights <- ifelse(dataset_ult$gender == 1,
                        (1/table(dataset_ult$gender)[1]) * 0.5,
                        (1/table(dataset_ult$gender)[2]) * 0.5)
n_repeats <- 5
n_folds <- 10

DT_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary, method = "repeatedcv", number = n_folds, repeats = n_repeats) 
DT_grid <- expand.grid(cp=c(0.0005,0.01,0.05,0.005,0.001,0.007)) # cp grid parameters are changed each time

# minbucket is changed manually 
project_DT_fit1 <- train(make.names(gender)~ .,data=dataset_ult, 
                         method ="rpart", weights = model_weights, 
                         metric = 'ROC',tuneGrid = DT_grid, trControl=DT_control, control = rpart.control(minbucket=10))
project_DT_fit1
project_DT_fit1$finalModel

prob1 <- round(predict(project_DT_fit1, newdata = project_test_ult, type='prob'), digits=3)
prob1
write.csv(prob1, file="p1.csv")

################################################################################
## Model2 - Random Forest - weighted resampling ################################
set.seed(1)
RF_grid <- expand.grid(mtry = c(3,5,7)) #parameters are changed each time
n_repeats=5
n_folds=10
RF_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats) 
project_RF_fit <- train(make.names(gender)~ .,data=dataset_ult, method ="rf", weights = model_weights, metric = 'ROC',tuneGrid = RF_grid, trControl=RF_control)

project_RF_fit
project_RF_fit$finalModel

prob2 <- round(predict(project_RF_fit, newdata = project_test_ult, type='prob'), digits=3)
prob2
write.csv(prob2, file="p2.csv")

################################################################################ 
## Model3 - Random Forest - up/smote/down/rose resampling ######################
#up resampling
set.seed(1)
RF_grid <- expand.grid(mtry = c(3,5,7,9)) #parameters are changed each time
n_repeats=5
n_folds=10
RF_control_up <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "up")
project_RF_fit_up <- train(make.names(gender)~ .,data=dataset_ult, method ="rf", metric = 'ROC',tuneGrid = RF_grid, trControl=RF_control)

project_RF_fit_up
project_RF_fit_up$finalModel

prob3.1 <- round(predict(project_RF_fit_up, newdata = project_test_ult, type='prob'), digits=3)
prob3.1
write.csv(prob3.1, file="p3.1.csv")

#smote resampling
RF_control_sm <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "smote")
project_RF_fit_sm <- train(make.names(gender)~ .,data=dataset_ult, method ="rf", metric = 'ROC',tuneGrid = RF_grid, trControl=RF_control)

project_RF_fit_sm
project_RF_fit_sm$finalModel

prob3.2 <- round(predict(project_RF_fit_sm, newdata = project_test_ult, type='prob'), digits=3)
prob3.2
write.csv(prob3.2, file="p3.2.csv")

#down resampling
RF_control_dw <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "down")
project_RF_fit_dw <- train(make.names(gender)~ .,data=dataset_ult, method ="rf", metric = 'ROC',tuneGrid = RF_grid, trControl=RF_control)

project_RF_fit_dw
project_RF_fit_dw$finalModel

prob3.3 <- round(predict(project_RF_fit_dw, newdata = project_test_ult, type='prob'), digits=3)
prob3.3
write.csv(prob3.3, file="p3.3.csv")

#rose resampling
set.seed(1)
RF_grid <- expand.grid(mtry = c(3,5,7,9)) #parameters are changed each time
n_repeats=5
n_folds=10
RF_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "rose") 
project_RF_fit_ro <- train(make.names(gender)~ .,data=dataset_ult, method ="rf", metric = 'ROC',tuneGrid = RF_grid, trControl=RF_control)

project_RF_fit_ro
project_RF_fit_ro$finalModel

prob3.4 <- round(predict(project_RF_fit_ro, newdata = project_test_ult, type='prob'), digits=3)
prob3.4
write.csv(prob3.4, file="p3.4.csv")

## Model4 - SGB - weighted resampling ##########################################
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(5,7), n.trees = c(400,600,800),shrinkage=c(0.01),n.minobsinnode =c(5,7)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats)
project_SGB_fit <- train(make.names(gender)~ .,data=dataset_ult, method ="gbm", weights = model_weights, metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit
project_SGB_fit$finalModel

prob4 <- round(predict(project_SGB_fit, newdata = project_test_ult, type='prob'), digits=3)
prob4
write.csv(prob4, file="p4.csv")

## Model5 - SGB - up/down/smote resampling #####################################
#up resampling
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(5,7), n.trees = c(400,600,800),shrinkage=c(0.01),n.minobsinnode =c(5,7)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "up")
project_SGB_fit_up <- train(make.names(gender)~ .,data=dataset_ult, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_up
project_SGB_fit_up$finalModel

prob5.1 <- round(predict(project_SGB_fit_up, newdata = project_test_ult, type='prob'), digits=3)
prob5.1
write.csv(prob5.1, file="p5.1.csv")

#down resampling
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(13,15), n.trees = c(400,600,800),shrinkage=c(0.01),n.minobsinnode =c(13,15)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, 
                            summaryFunction=twoClassSummary,  
                            method = "repeatedcv", number = n_folds, 
                            repeats = n_repeats, sampling = "down")
project_SGB_fit_do <- train(make.names(gender)~ .,data=dataset_ult, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_do
project_SGB_fit_do$finalModel

prob5.2 <- round(predict(project_SGB_fit_do, newdata = project_test_ult, type='prob'), digits=3)
prob5.2
write.csv(prob5.2, file="p5.2.csv")

#smote resampling
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(7,9), n.trees = c(400,600,800),shrinkage=c(0.01),n.minobsinnode =c(7,9)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats, sampling = "smote")
project_SGB_fit_sm <- train(make.names(gender)~ .,data=dataset_ult, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_sm
project_SGB_fit_sm$finalModel

prob5.3 <- round(predict(project_SGB_fit_sm, newdata = project_test_ult, type='prob'), digits=3)
prob5.3
write.csv(prob5.3, file="p5.3.csv")

#*******************************************************************************
### Outside Resampling Techniques

#* over (or up) resampling
#* under (or down) resampling
#* both (over + under) resampling
#* rose resampling
#*******************************************************************************
## checking F and M distribution 
table(dataset_ult$gender) #(F(1)=3679, M(0)=1939, Total=5618)
#*******************************************************************************
# over sampling (3679*2=7358)
set.seed(1)
over_train <- ovun.sample(make.names(gender) ~ ., dataset_ult, 
                          method = "over",N = 7358)$data
table(over_train$gender)

## model7 - SGB - over (outside) resampling #################################### 
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(9,11), n.trees = c(700,800,900),shrinkage=c(0.01),n.minobsinnode =c(9,11)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats)
project_SGB_fit_o<- train(make.names(gender)~ .,data=over_train, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_o
project_SGB_fit_o$finalModel

prob7 <- round(predict(project_SGB_fit_o, newdata = project_test_ult, type='prob'), digits=3)
prob7
write.csv(prob7, file="p7.csv")

#*******************************************************************************
# under resampling (1939*2=3878) 
set.seed(1)
under_train <- ovun.sample(make.names(gender) ~ ., dataset_ult, method = "under",N = 3878)$data
table(under_train$gender)
#*******************************************************************************
## model8 - SGB - under resampling #############################################
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(9,11), n.trees = c(700,800,900),shrinkage=c(0.01),n.minobsinnode =c(9,11)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats)
project_SGB_fit_un<- train(make.names(gender)~ .,data=under_train, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_un
project_SGB_fit_un$finalModel

prob8 <- round(predict(project_SGB_fit_un, newdata = project_test_ult, type='prob'), digits=3)
prob8
write.csv(prob8, file="p8.csv")
#*******************************************************************************
# both (over+under) resampling (1939+3679=5618) 
set.seed(1)
both_train <- ovun.sample(make.names(gender) ~ ., dataset_ult, method = "both", N = 5618, p=0.5)$data
table(both_train$gender) #(0=2757, 1=2861)
#*******************************************************************************
## model9 - SGB - both (over+under) resampling ################################# 
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(9,11), n.trees = c(700,800,900),shrinkage=c(0.01),n.minobsinnode =c(9,11)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats)
project_SGB_fit_b<- train(make.names(gender)~ .,data=both_train, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)

project_SGB_fit_b
project_SGB_fit_b$finalModel

prob9 <- round(predict(project_SGB_fit_b, newdata = project_test_ult, type='prob'), digits=3)
prob9
write.csv(prob9, file="p9.csv")
#*******************************************************************************
# rose sampling
set.seed(1)
rose_train <- ROSE(make.names(gender) ~ ., data  = dataset_ult)$data          
table(rose_train$gender)
#*******************************************************************************
## model10 - SGB - rose resampling ###################################################
set.seed(1)
n_folds=10
n_repeats=5
SGB_grid <- expand.grid(interaction.depth=c(9), n.trees = c(800),shrinkage=c(0.01),n.minobsinnode =c(9)) #parameters are changed each time
SGB_control <- trainControl(verboseIter=TRUE, classProbs=TRUE, summaryFunction=twoClassSummary,  method = "repeatedcv", number = n_folds, repeats = n_repeats)
project_SGB_fit_r <- train(make.names(gender)~ .,data=rose_train, method ="gbm", metric = 'ROC',tuneGrid = SGB_grid, trControl=SGB_control)
project_SGB_fit_r

project_SGB_fit_r$finalModel

prob10 <- round(predict(project_SGB_fit_r, newdata = project_test_ult, type='prob'), digits=3)
prob10
write.csv(prob10, file="p10.csv")
####****************************************************************************
#summary of the best SGB model with out-over sampling - most influencing parameters
summary(project_SGB_fit_o$finalModel, 
        cBars = 10,method = relative.influence, las = 1)
####****************************************************************************
