### Done by Szymczak Wojciech 

rm(list = ls())
setwd("C:/Users/ASUS/Dropbox/WSZ/Machine Learning/Regression_assessment")
set.seed(424178) #set seed for comparable results - my index number


## libraries ##
library(caret)
library(tibble)
library(purrr)
library(corrplot)
library(DescTools)
library(tidyverse)
library(readr)
library(olsrr)
library(AER)
library(lmtest) 
library(nnet)
library(bestNormalize)
library(mixgb)
library(smbinning)

## functions
houses_F_anova <- function(categorical_var) {
  anova_ <- aov(data_internal_train$newborn_weight ~ 
                  data_internal_train[[categorical_var]]) 
  
  return(summary(anova_)[[1]][1, 4])
}

regressionMetrics <- function(real, predicted) {
  # Mean Square Error
  MSE <- mean((real - predicted)^2)
  # Root Mean Square Error
  RMSE <- sqrt(MSE)
  # Mean Absolute Error
  MAE <- mean(abs(real - predicted))
  # Mean Absolute Percentage Error
  MAPE <- mean(abs(real - predicted)/real)
  # Median Absolute Error
  MedAE <- median(abs(real - predicted))
  # R2
  R2 <- cor(predicted, real)^2
  
  result <- data.frame(MSE, RMSE, MAE, MAPE, MedAE, R2)
  return(result)
}


## controls for validation
ctrl_cv5 <- trainControl(method = "cv",
                         number = 5)

# Read the data 
data_train <- read.csv("newborn_train.csv")
data_test <- read.csv("newborn_test.csv")

#Internal Division of the train data for validation
data_internal_division <- createDataPartition(data_train$newborn_weight,
                                          p = 0.7, 
                                          list = FALSE)

data_internal_train <- data_train[data_internal_division,]
data_internal_validation <- data_train[-data_internal_division,]

## distribution of target variable in both samples - same variables
summary(data_internal_train$newborn_weight)
summary(data_internal_validation$newborn_weight)

#### DATA DESCRIPTION ######

# mother_body_mass_index – Body Mass Index of the mother
# mother_marital_status – is mother married? (1 = Yes, 2 = No)
# mother_delivery_weight – mother’s weight at delivery in pounds
# mother_race – race of the mother (1 = White (alone); 2 = Black (alone); 3 = AIAN (alone); 4 Asian (alone); 5 = NHOPI (alone); 6 = More than one race)
# mother_height – height of the mother in inches
# mother_weight_gain – mother’s weight gain during the pregnancy (in pounds)
# father_age – age of the father
# father_education – education of the father (1 = 8th grade or less; 2 = 9-12th grade, no diploma; 3 = High school graduate or GED completed; 4 = Some college credit but no degree; 5 = Associate degree; 6 = Bachelor’s degree; 7 = Master’s degree; 8 = Doctorate or Professional degree; 9 = unknown)
# cigarettes_before_pregnancy – number of cigarettes smoked daily by the mother before pregnancy (00-97 = number of cigarettes daily, 98 = 98 or more cigarettes daily)
# prenatal_care_month – pregnancy month in which prenatal care began (99 = no prenatal care)
# number_prenatal_visits – number of prenatal visits
# previous_cesarean – was there any previous cesarean delivery before (N = No; Y = Yes; U = Other)
# newborn_gender – gender of the newborn (F = female; M = male)

# code as factor:
data_internal_train$mother_marital_status <- as.factor(data_internal_train$mother_marital_status) 
data_internal_train$mother_race <- as.factor(data_internal_train$mother_race) 
data_internal_train$previous_cesarean <- as.factor(data_internal_train$previous_cesarean)
data_internal_train$newborn_gender <- as.factor(data_internal_train$newborn_gender)

# nprenatal_visit
# recode
quantiles <- quantile(data_internal_train$number_prenatal_visits, probs = seq(0, 1, by = 1/4), na.rm = TRUE)
# Create a new variable 'nprenatal_visit' with quantile groups
data_internal_train$nprenatal_visit <- cut(data_internal_train$number_prenatal_visits, breaks = quantiles, labels = FALSE, include.lowest = TRUE)
data_internal_train$previous_cesarean[data_internal_train$previous_cesarean=='U'] <- 'N'
data_internal_train$father_edu <- 1 
data_internal_train$father_edu[data_internal_train$father_education == 3 | data_internal_train$father_education == 4] <- 2 
data_internal_train$father_edu[data_internal_train$father_education == 5 | data_internal_train$father_education == 6 | data_internal_train$father_education == 7 | data_internal_train$father_education == 8] <- 3 
table(data_internal_train$father_edu)
data_internal_train$no_prenatal_care_month <- 0
data_internal_train$no_prenatal_care_month[data_internal_train$prenatal_care_month == 99] <- 1
data_internal_train$parenatal_care_after3months <- 0
data_internal_train$parenatal_care_after3months[ data_internal_train$prenatal_care_month > 3 & data_internal_train$prenatal_care_month < 99] = 1 
data_internal_train$no_cig <- 0
data_internal_train$no_cig[data_internal_train$cigarettes_before_pregnancy==0] <- 1
data_internal_train$median_cig <- 0
data_internal_train$median_cig[data_internal_train$cigarettes_before_pregnancy > 20] <- 1
data_internal_train$mother_body_mass_index2 <- data_internal_train$mother_body_mass_index * data_internal_train$mother_body_mass_index
data_internal_train$mother_weight_gain2 <- data_internal_train$mother_weight_gain * data_internal_train$mother_weight_gain
data_internal_train$mother_delivery_weight2 <- data_internal_train$mother_delivery_weight * data_internal_train$mother_delivery_weight
data_internal_train$mother_height2 <- data_internal_train$mother_height * data_internal_train$mother_height
data_internal_train$father_age2 <- data_internal_train$father_age * data_internal_train$father_age

data_internal_train %>% 
  sapply(is.numeric) %>% 
  which()
  
for (i in c(1,3,5,6,7,11)) {
  q1=quantile(data_internal_train[,i], .25, na.rm = T) # 1st quantile
  q3=quantile(data_internal_train[,i], .75, na.rm = T) # 3rd quantile
  IQR=IQR(data_internal_train[,i], na.rm = T) # calculating IQR
  up<-(q1 - 1.5 * IQR)
  down<-(q3 + 1.5 * IQR)
  data_internal_train$check<-ifelse(data_internal_train[,i]<down & data_internal_train[,i]>up,'no','yes') # If yes, it is outlier. If no, it is not outlier.
  data_internal_train[,i][data_internal_train$check=='yes']<-median(data_internal_train[,i][data_internal_train$check=='no']) # outliers is replaced by median
}


# first lm to check 
data_names <- data_internal_train %>%
  names()
data_names_first <- data_names[c(-8,-9,-10,-11, -26, -27)]


model_lm_final <- lm(newborn_weight ~ ., 
                     data = as.data.frame(data_internal_train) %>% 
                       dplyr::select(all_of(c(data_names_first))))
summary(model_lm_final)

## replace all NAs with imputaion based on XGBoost:
imputed.data <- mixgb(data = data_internal_train %>% 
                        dplyr::select(all_of(c(data_names_first))), m = 1, 
                      save.models = TRUE,
                      save.vars = colnames(data_internal_train %>% 
                                             dplyr::select(all_of(c(data_names_first)))))

#merge_datasets
data_internal_train2 <- as.data.frame(imputed.data$imputed.data[[1]])
colSums(is.na(data_internal_train2))
save(data_internal_train2, file = "imputed_mixgb.RData")
data_internal_train2

data_names2 <- data_internal_train2 %>%
  names()


data_names_first2 <- data_names2[c(-8,-9,-10,-11, -20)]

model_first <- lm(newborn_weight ~., 
                  data = data_internal_train2 %>%
  select(all_of(data_names2)))

regressionMetrics(data_internal_train2$newborn_weight, predict(model_first)) #0.1608147  

##add meeded information to validation and test data
##VALIDATION##
quantiles <- quantile(data_internal_train$number_prenatal_visits, probs = seq(0, 1, by = 1/4), na.rm = TRUE)
# Create a new variable 'nprenatal_visit' with quantile groups
data_internal_validation$nprenatal_visit <- cut(data_internal_validation$number_prenatal_visits, breaks = quantiles, labels = FALSE, include.lowest = TRUE)
data_internal_validation$previous_cesarean[data_internal_validation$previous_cesarean=='U'] <- 'N'
data_internal_validation$father_edu <- 1 
data_internal_validation$father_edu[data_internal_validation$father_education == 3 | data_internal_validation$father_education == 4] <- 2 
data_internal_validation$father_edu[data_internal_validation$father_education == 5 | data_internal_validation$father_education == 6 | data_internal_validation$father_education == 7 | data_internal_validation$father_education == 8] <- 3 
table(data_internal_validation$father_edu)
data_internal_validation$no_prenatal_care_month <- 0
data_internal_validation$no_prenatal_care_month[data_internal_validation$prenatal_care_month == 99] <- 1
data_internal_validation$parenatal_care_after3months <- 0
data_internal_validation$parenatal_care_after3months[ data_internal_validation$prenatal_care_month > 3 & data_internal_validation$prenatal_care_month < 99] = 1 
data_internal_validation$no_cig <- 0
data_internal_validation$no_cig[data_internal_validation$cigarettes_before_pregnancy==0] <- 1
data_internal_validation$median_cig <- 0
data_internal_validation$median_cig[data_internal_validation$cigarettes_before_pregnancy > 20] <- 1
data_internal_validation$mother_body_mass_index2 <- data_internal_validation$mother_body_mass_index * data_internal_validation$mother_body_mass_index
data_internal_validation$mother_weight_gain2 <- data_internal_validation$mother_weight_gain * data_internal_validation$mother_weight_gain
data_internal_validation$mother_delivery_weight2 <- data_internal_validation$mother_delivery_weight * data_internal_validation$mother_delivery_weight
data_internal_validation$mother_height2 <- data_internal_validation$mother_height * data_internal_validation$mother_height
data_internal_validation$father_age2 <- data_internal_validation$father_age * data_internal_validation$father_age

data_internal_validation %>% 
  sapply(is.numeric) %>% 
  which()

for (i in c(1,3,5,6,7,11)) {
  q1=quantile(data_internal_validation[,i], .25, na.rm = T) # 1st quantile
  q3=quantile(data_internal_validation[,i], .75, na.rm = T) # 3rd quantile
  IQR=IQR(data_internal_validation[,i], na.rm = T) # calculating IQR
  up<-(q1 - 1.5*IQR)
  down<-(q3 + 1.5*IQR)
  data_internal_validation$check<-ifelse(data_internal_validation[,i]<down & data_internal_validation[,i]>up,'no','yes') # If yes, it is outlier. If no, it is not outlier.
  data_internal_validation[,i][data_internal_validation$check=='yes']<-median(data_internal_validation[,i][data_internal_validation$check=='no']) # outliers is replaced by median
}

data_internal_validation$mother_marital_status <- as.factor(data_internal_validation$mother_marital_status) 
data_internal_validation$mother_race <- as.factor(data_internal_validation$mother_race) 
data_internal_validation$previous_cesarean <- as.factor(data_internal_validation$previous_cesarean)
data_internal_validation$newborn_gender <- as.factor(data_internal_validation$newborn_gender)

##TEST DATA ##
# code as factor:
data_test$mother_marital_status <- as.factor(data_test$mother_marital_status) 
data_test$mother_race <- as.factor(data_test$mother_race) 
data_test$previous_cesarean <- as.factor(data_test$previous_cesarean)
data_test$newborn_gender <- as.factor(data_test$newborn_gender)

# nprenatal_visit
# recode
quantiles <- quantile(data_test$number_prenatal_visits, probs = seq(0, 1, by = 1/4), na.rm = TRUE)
# Create a new variable 'nprenatal_visit' with quantile groups
data_test$nprenatal_visit <- cut(data_test$number_prenatal_visits, breaks = quantiles, labels = FALSE, include.lowest = TRUE)
data_test$previous_cesarean[data_test$previous_cesarean=='U'] <- 'N'
data_test$father_edu <- 1 
data_test$father_edu[data_test$father_education == 3 | data_test$father_education == 4] <- 2 
data_test$father_edu[data_test$father_education == 5 | data_test$father_education == 6 | data_test$father_education == 7 | data_test$father_education == 8] <- 3 
table(data_test$father_edu)
data_test$no_prenatal_care_month <- 0
data_test$no_prenatal_care_month[data_test$prenatal_care_month == 99] <- 1
data_test$parenatal_care_after3months <- 0
data_test$parenatal_care_after3months[ data_test$prenatal_care_month > 3 & data_test$prenatal_care_month < 99] = 1 
data_test$no_cig <- 0
data_test$no_cig[data_test$cigarettes_before_pregnancy==0] <- 1
data_test$median_cig <- 0
data_test$median_cig[data_test$cigarettes_before_pregnancy > 20] <- 1
data_test$mother_body_mass_index2 <- data_test$mother_body_mass_index * data_test$mother_body_mass_index
data_test$mother_weight_gain2 <- data_test$mother_weight_gain * data_test$mother_weight_gain
data_test$mother_delivery_weight2 <- data_test$mother_delivery_weight * data_test$mother_delivery_weight
data_test$mother_height2 <- data_test$mother_height * data_test$mother_height
data_test$father_age2 <- data_test$father_age * data_test$father_age


data_names_first <- data_internal_validation%>% names()

data_test <- data_test %>% 
  dplyr::select(all_of(c(data_names_first[c(-8,-9,-10,-11,-14, -26)]))) #get rid of newborn_weight

#correlations
data_numeric <- data_internal_train2 %>% 
  select(all_of(data_names2)) %>% 
  sapply( is.numeric) %>% 
  # select those which are
  which() %>% 
  # and keep just their names
  names()

data_correlations <- 
  cor(data_internal_train2[, data_numeric],
      use = "pairwise.complete.obs")

findCorrelation(data_correlations, 
                cutoff = 0.75, 
                names = TRUE)

# check near zero variance
nearZeroVar(data_internal_train2 %>% 
              dplyr::select(all_of(data_names2)),
            saveMetrics = TRUE) -> nzv_data

nzv_data %>% 
  # we add rownames of the frame
  # (with names of variables)
  # as a new column in the data
  rownames_to_column("variable") %>% 
  # and sort it in the descreasing order;
  arrange(-zeroVar, -nzv, -freqRatio)

# data internal train get rid of the additional variables 
data_internal_train3 <- data_internal_train2 %>% 
  select(c(-median_cig,
           -no_prenatal_care_month,
           -mother_weight_gain2,
           -mother_body_mass_index2,
           -mother_delivery_weight2,
           -father_age2,
           -mother_height2)) 

#In training will use pca, nzv, scale and center pre-processing

##run the code
model_lm_fin <- lm(newborn_weight ~ ., 
                   data = data_internal_train2)

regressionMetrics(real =  data_internal_train2$newborn_weight,
                  predicted = predict(model_lm_fin,
                                      data_internal_train2)) #0.1608147  
summary(model_lm_fin)

## apply stepwise selection
model_step_final <- ols_step_forward_aic(model_lm_fin)

summary(model_step_final$model)
regressionMetrics(real =  data_internal_train2$newborn_weight,
                  predicted = predict(model_step_final$model,
                                      data_internal_train2)) #0.1608147
## apply lasso
# LASSO
parameters_lasso <- expand.grid(alpha = 1,
                                lambda = seq(10, 1e4, 10))


regression_lasso <-  train(newborn_weight ~ ., 
                           data = data_internal_train2 %>% 
                             dplyr::select(all_of(data_names2)),
                           method = "glmnet",
                           preProcess = c("nzv", "center", "pca", "scale"),
                           tuneGrid = parameters_lasso,
                           trControl = ctrl_cv5)


regressionMetrics(real = data_internal_train2$newborn_weight,
                  predicted = predict(regression_lasso,
                                      data_internal_train2 %>% 
                                        dplyr::select(all_of(data_names2)))) #0.1618197  LASSO

## apply ridge 
lambdas <- exp(log(10)*seq(-2, 9, length.out = 200))

parameters_ridge <- expand.grid(alpha = 0, # ridge 
                                lambda = lambdas)

ridge_regression <- train(newborn_weight ~ .,
                          data = as.data.frame(data_internal_train2 %>% 
                            dplyr::select(all_of(data_names2))),
                          method = "glmnet",
                          preProc = c("nzv", "center", "pca", "scale"),
                          tuneGrid = parameters_ridge,
                          trControl = ctrl_cv5)


regressionMetrics(real = data_internal_train$newborn_weight,
                  predicted = predict(ridge_regression, 
                                      data_internal_train2 %>% 
                                        dplyr::select(all_of(data_names2)))) #0.1614087 ridge

## Elastic Net
parameters_elastic <- expand.grid(alpha = seq(0, 1, 0.2), 
                                  lambda = seq(10, 1e4, 10))
set.seed(424178)

elasticn_regression <- train(newborn_weight ~ ., 
                        data = data_internal_train2 %>% 
                          dplyr::select(all_of(data_names2)),
                        method = "glmnet",
                        preProc = c("nzv", "center", "pca", "scale"),
                        tuneGrid = parameters_elastic,
                        trControl = ctrl_cv5)

regressionMetrics(real = data_internal_train2$newborn_weight,
                  predicted = predict(elasticn_regression,
                                      data_internal_train2 %>% 
                                        dplyr::select(all_of(data_names2)))) #0.1614087 

# XGBoost Random Forest
tune_grid <- expand.grid(nrounds = 200,
                         max_depth = 5,
                         eta = 0.05,
                         gamma = 0.01,
                         colsample_bytree = 0.75,
                         min_child_weight = 0,
                         subsample = 0.5)


rf_fit <- train(newborn_weight ~ ., 
                data = data_internal_train2 %>% 
                  dplyr::select(all_of(data_names2)),
                method = "xgbTree",
                preProc = c("nzv", "center", "pca", "scale"),
                tuneGrid = tune_grid,
                trControl = ctrl_cv5,
                tuneLength = 10)


regressionMetrics(real = data_internal_train2$newborn_weight,
                  predicted = predict(rf_fit,
                                      data_internal_train2 %>% 
                                        dplyr::select(all_of(data_names2)))) # MAPE 0.1586538 




#check on validation
prediction_linear <- predict(model_lm_fin, 
                             newdata = data_internal_validation)

regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_linear[complete.cases(data_internal_validation)]) # MAPE 0.1507628 

prediction_step <- predict(model_step_final$model, 
                           newdata = data_internal_validation)

regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_step[complete.cases(prediction_step)]) # MAPE 0.1507628 


prediction_ridge <- predict(ridge_regression, 
                           newdata = data_internal_validation)

regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_ridge[complete.cases(prediction_ridge)]) # MAPE 0.1509835 

prediction_lasso <- predict(regression_lasso, 
                            newdata = data_internal_validation)

regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_lasso[complete.cases(prediction_lasso)]) # MAPE 0.1509835 

prediction_elastic <- predict(elasticn_regression, 
                            newdata = data_internal_validation)

regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_elastic[complete.cases(prediction_elastic)]) # MAPE 0.1509835  


prediction_xgrf <- predict(rf_fit, 
                             newdata = data_internal_validation)


regressionMetrics(real = data_internal_validation$newborn_weight[complete.cases(data_internal_validation)],
                  predicted = prediction_xgrf[complete.cases(prediction_xgrf)]) # MAPE 0.1491029 

# predict test and export
# Create a dummy outcome variable in the test dataset


#fill with median 
options(contrasts = c("contr.treatment",  # for non-ordinal factors
                      "contr.treatment")) # for ordinal factors
colSums(is.na(data_test))
data_test$mother_body_mass_index[is.na(data_test$mother_body_mass_index)] <- median(data_test$mother_body_mass_index, na.rm = TRUE)       
data_test$mother_marital_status[is.na(data_test$mother_marital_status)] <- "1"
data_test$mother_delivery_weight[is.na(data_test$mother_delivery_weight)] <- median(data_test$mother_delivery_weight, na.rm = TRUE)       
data_test$mother_height[is.na(data_test$mother_height)] <- median(data_test$mother_height, na.rm = TRUE)
data_test$mother_weight_gain[is.na(data_test$mother_weight_gain)] <- median(data_test$mother_weight_gain, na.rm = TRUE)
data_test$father_age[is.na(data_test$father_age)] <- median(data_test$father_age, na.rm = TRUE)
data_test$nprenatal_visit[is.na(data_test$nprenatal_visit)] <- 2
data_test$mother_body_mass_index2[is.na(data_test$mother_body_mass_index2)] <- median(data_test$mother_body_mass_index2, na.rm = TRUE)
data_test$mother_weight_gain2[is.na(data_test$mother_weight_gain2)] <- median(data_test$mother_weight_gain2, na.rm = TRUE)
data_test$mother_delivery_weight2[is.na(data_test$mother_delivery_weight2)] <- median(data_test$mother_delivery_weight2, na.rm = TRUE)
data_test$mother_height2[is.na(data_test$mother_height2)] <- median(data_test$mother_height2, na.rm = TRUE)
data_test$father_age2[is.na(data_test$father_age2)] <- median(data_test$father_age2, na.rm = TRUE)


predicted_final<-predict(rf_fit, newdata = data_test )
write.csv(predicted_final, file = "final_prediction.csv", fileEncoding = 'UTF-8')


