options(scipen=999)

### Packages
library(tidyverse)
library(caret)
library(dplyr)
library(writexl)

### Data import 
client_df<-read.csv("data/client_attrition_train.csv") 
client_test<-read.csv("data/client_attrition_test.csv")
client_df<-client_df %>% select(-customer_id) # remove unnecessary column
client_test<-client_test %>% select(-customer_id) # remove unnecessary column

### Convert character variable into factor variable
client_df$customer_sex<-factor(client_df$customer_sex,ordered = F,levels = c(unique(client_df$customer_sex)))
client_df$customer_education<-factor(client_df$customer_education,ordered = F,levels = c(unique(client_df$customer_education)))
client_df$customer_civil_status<-factor(client_df$customer_civil_status,ordered = F,levels = c(unique(client_df$customer_civil_status)))
client_df$customer_salary_range<-factor(client_df$customer_salary_range,ordered = F,levels = c(unique(client_df$customer_salary_range)))
client_df$credit_card_classification<-factor(client_df$credit_card_classification,ordered = F,levels = c('Blue','Silver','Gold','Platinum'))
client_df$account_status<-factor(client_df$account_status,ordered = F,levels = c(unique(client_df$account_status)))
client_test$customer_sex<-factor(client_test$customer_sex,ordered = F,levels = c(unique(client_test$customer_sex)))
client_test$customer_education<-factor(client_test$customer_education,ordered = F,levels = c(unique(client_test$customer_education)))
client_test$customer_civil_status<-factor(client_test$customer_civil_status,ordered = F,levels = c(unique(client_test$customer_civil_status)))
client_test$customer_salary_range<-factor(client_test$customer_salary_range,ordered = F,levels = c(unique(client_test$customer_salary_range)))
client_test$credit_card_classification<-factor(client_test$credit_card_classification,ordered = F,levels = c('Blue','Silver','Gold','Platinum'))

### Filling missing value
client_df$customer_salary_range[is.na(client_df$customer_salary_range)]<-'Unknown' # null customer_salary_range converted into 'unknown'
client_df$customer_age[is.na(client_df$customer_age)]<-median(client_df$customer_age,na.rm = T) # fill missing value as median value
client_df$total_transaction_amount[is.na(client_df$total_transaction_amount)]<-median(client_df$total_transaction_amount,na.rm = T) # fill missing value as median value
client_test$customer_salary_range[is.na(client_test$customer_salary_range)]<-'Unknown' # null customer_salary_range converted into 'unknown
client_test$customer_age[is.na(client_test$customer_age)]<-median(client_test$customer_age,na.rm = T) # fill missing value as median value
client_test$total_transaction_amount[is.na(client_test$total_transaction_amount)]<-median(client_test$total_transaction_amount,na.rm = T) # fill missing value as median value

### Outliers - All outliers replaced by median value for each numeric columns
for (i in c(1,3,7,8,10,11,12,13,14,15,16,17,18,19)) {
  q1=quantile(client_df[,i], .25) # 1st quantile
  q3=quantile(client_df[,i], .75) # 3rd quantile
  IQR=IQR(client_df[,i]) # calculating IQR
  up<-(q1 - 1.5*IQR)
  down<-(q3 + 1.5*IQR)
  client_df$check<-ifelse(client_df[,i]<down & client_df[,i]>up,'no','yes') # If yes, it is outlier. If no, it is not outlier.
  client_df[,i][client_df$check=='yes']<-median(client_df[,i][client_df$check=='no']) # outliers is replaced by median
}

for (i in c(1,3,7,8,10,11,12,13,14,15,16,17,18,19)) {
  q1=quantile(client_test[,i], .25)
  q3=quantile(client_test[,i], .75)
  IQR=IQR(client_test[,i])
  up<-(q1 - 1.5*IQR)
  down<-(q3 + 1.5*IQR)
  client_test$check<-ifelse(client_test[,i]<down & client_test[,i]>up,'no','yes')
  client_test[,i][client_test$check=='yes']<-median(client_test[,i][client_test$check=='no'])
}

### Normalize all numeric variables
for (i in c(1,3,7,8,10,11,12,13,14,15,16,17,18,19)) {
  client_df[,i]<-scale(client_df[,i])
}
for (i in c(1,3,7,8,10,11,12,13,14,15,16,17,18,19)) {
  client_test[,i]<-scale(client_test[,i])
}
client_df=client_df %>% select(-check) # remove unnecessary variable from previous step
client_test=client_test %>% select(-check) # remove unnecessary variable from previous step

### Data split. Client_train is divided into train and validation set.
split <- createDataPartition(client_df$account_status, 
                             p = 0.7, # 70% and 30% ratio
                             list = FALSE)
client_train <- client_df[c(split),] # train set
client_valid <- client_df[-c(split),] # validation set
table(client_train$account_status) # imbalanced data of train set
table(client_valid$account_status) # imbalanced data of validation set


### Modeling - best model: GBM classifier
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5,
                         classProbs = TRUE)
options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors
client_train<-client_train %>% select(-customer_available_credit_limit) # Remove customer_available_credit_limit variable based on correlation analysis
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), # create hyperparameter's space
                        n.trees = c(50,100,150), 
                        shrinkage = c(0.1,0.01,0.001),
                        n.minobsinnode = 10)
ctrl_cv5$sampling<-"up" # resampling up method
sgb_model_up <- train(account_status ~ ., # train gbm with up resampling method
                      data = client_train %>% 
                        dplyr::select(-customer_sex), # remove customer sex variable based on missing value analysis. it has much missing value and high correlation with other variables
                      method = "gbm",distribution='bernoulli', 
                      trControl = ctrl_cv5,tuneGrid=gbmGrid,train.fraction=0.7)
ctrl_cv5$sampling<-"smote" # resampling smote method
gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 10), # create hyperparameter's space
                        n.trees = c(50,100,150), 
                        shrinkage = c(0.1,0.01,0.001),
                        n.minobsinnode = 10)
sgb_model_smote <- train(account_status ~ .,
                         data = client_train %>% 
                           dplyr::select(-customer_sex), # remove customer sex variable based on missing value analysis. it has much missing value and high correlation with other variables
                         method = "gbm",distribution='bernoulli',
                         trControl = ctrl_cv5,tuneGrid=gbmGrid,train.fraction=0.7) 

predict_sgb_up<-predict(sgb_model_up, client_train) # predict train data using gbm with up 
predict_sgb_smote<-predict(sgb_model_smote, client_train) # predict train data using gbm with smote
confusionMatrix(predict_sgb_up, client_train$account_status) # performance metrics of gbm(up) model on train data                  
confusionMatrix(predict_sgb_smote, client_train$account_status) # performance metrics of gbm(smote) model on test data

predict_boost_up_test<-predict(sgb_model_up, client_valid) # predict valid data using gbm with up
predict_boost_smote_test<-predict(sgb_model_smote, client_valid) # predict valid data using gbm with smote
confusionMatrix(predict_boost_up_test, client_valid$account_status) # performance metrics of gbm(up) model on validation data
confusionMatrix(predict_boost_smote_test, client_valid$account_status) # performance metrics of gbm(smote) model on validation data
 
############ Best model
# Gradient boosing model with up resampling method is the best model.
# Because it has the highest balanced accuracy on test set
Best_Model_Predict<-predict(sgb_model_up,client_test) # predict test data using best model
write.csv(Best_Model_Predict,"Predict_classification.csv") # export predicted value of test data using best model

