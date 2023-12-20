Sys.setenv(LANG = "en")
options(scipen = 5)

###-------------------------Packages-------------------------
library(tidyverse)
library(dplyr)
library(caret)
library("sandwich")
library("lmtest")
library("MASS")
library("mfx")
library("htmltools")
library("aod")
library(DescTools)
library(aods3)
library("logistf")

###-------------------------Data import-------------------------
library(readr)
CAX_Startup_Data_Dictionary <- read.csv2("CAX_Startup_Data_Dictionary.csv",header=T,sep=',') # dictionary file of data set
df <- read.csv2("CAX_Startup_Data.csv",header = T,sep=',') # data set
df$Local.or.global.player=trimws(df$Local.or.global.player) # Removing white spaces from chosen column
df=df %>% dplyr::select( # Target variable ### Initial variable selection by 4 categories
                          Dependent.Company.Status,
                          # Founder characteristics
                          Worked.in.top.companies,Average.size.of.companies.worked.for.in.the.past,
                          Have.been.part.of.startups.in.the.past., Have.been.part.of.successful.startups.in.the.past.,
                          Consulting.experience.,Was.he.or.she.partner.in.Big.5.consulting.,
                          # Startup characteristics
                          Age.of.company.in.years,Focus.on.private.or.public.data., Focus.on.consumer.data.,
                          Focus.on.structured.or.unstructured.data, Machine.Learning.based.business, Predictive.Analytics.business,
                          Speech.analytics.business, Prescriptive.analytics.business,
                          Big.Data.Business, Product.or.service.company.,Local.or.global.player, 
                          # Access to capital
                          Last.Funding.Amount,
                          # External factors
                          Number.of.Investors.in.Seed,Internet.Activity.Score,Number.of.Co.founders, Number.of.of.advisors, Team.size.all.employees,
                          Number.of..Sales.Support.material,Number.of..of.Partners.of.company,
                        ) # selecting columns/variables

###-------------------------Data prepare-------------------------
# Categorical variables
df <- df %>% # Replace High value to high in chosen column
  mutate(Number.of..Sales.Support.material = str_replace(Number.of..Sales.Support.material,"High","high"))
df <- df %>%  # Replace no value to Both value in chosen column
  mutate(Focus.on.structured.or.unstructured.data = str_replace(Focus.on.structured.or.unstructured.data,"no","Both"))
df <- df %>%  # Replace not applicable value to Both value in chosen column
  mutate(Focus.on.structured.or.unstructured.data = str_replace(Focus.on.structured.or.unstructured.data,"not applicable","Both"))
df <- df %>%  # Replace both applicable value to Both value in chosen columns
  mutate(Focus.on.structured.or.unstructured.data = str_replace(Focus.on.structured.or.unstructured.data,"Botht applicable","Both"))
df$Local.or.global.player<-tolower(df$Local.or.global.player) # convert into lowercase or lower letter of chosen column
for (i in c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,26)) { # Replace No Info value of all categorical variables by the most frequently value
  new_val<-names(which.max(table(df[,i])))
  df[,i][df[,i] == 'No Info'] <- new_val
}
for (i in c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,19,26)) { # Replace no info value of all categorical variables by the most frequently value
  new_val<-names(which.max(table(df[,i])))
  df[,i][df[,i] == 'no info'] <- new_val
}
df$Number.of..Sales.Support.material[df$Number.of..Sales.Support.material=='Nothing']<-names(which.max(table(df$Number.of..Sales.Support.material))) # Replace Nothing value of chosen column into the most frequently value
df$Number.of..Sales.Support.material=factor(df$Number.of..Sales.Support.material,ordered = T,levels = c('low','medium','high')) # apply ordered factor type for chosen variable
df$Average.size.of.companies.worked.for.in.the.past=factor(df$Average.size.of.companies.worked.for.in.the.past,ordered = T,levels = c('Small','Medium','Large')) # apply ordered factor type for chosen variable
df$Number.of..of.Partners.of.company=factor(df$Number.of..of.Partners.of.company,ordered = T,levels = c('None','Few','Many')) # apply ordered factor type for chosen variable
df=df %>% mutate_at(vars(Worked.in.top.companies,
                         Have.been.part.of.startups.in.the.past.,
                         Have.been.part.of.successful.startups.in.the.past.,
                         Consulting.experience.,
                         Product.or.service.company.,
                         Focus.on.private.or.public.data.,
                         Focus.on.consumer.data.,
                         Focus.on.structured.or.unstructured.data,
                         Local.or.global.player,
                         Machine.Learning.based.business,
                         Predictive.Analytics.business,
                         Speech.analytics.business,
                         Prescriptive.analytics.business,
                         Big.Data.Business,
                         Was.he.or.she.partner.in.Big.5.consulting.),as.factor) # apply not ordered factor type for chosen variables
# Numerical variables
df=df %>% mutate_at(vars(Age.of.company.in.years,
                      Internet.Activity.Score,
                      Last.Funding.Amount,
                      Number.of.Investors.in.Seed,
                      Number.of.Co.founders,
                      Number.of.of.advisors,
                      Team.size.all.employees),as.numeric) # apply numeric type for chosen variables

# Missing values
colSums(is.na(df)) %>% 
  sort() # sorted number of missing value of all columns
df$Average.size.of.companies.worked.for.in.the.past[is.na(df$Average.size.of.companies.worked.for.in.the.past)] <- names(which.max(table(df$Average.size.of.companies.worked.for.in.the.past)))
df$Number.of.Investors.in.Seed[is.na(df$Number.of.Investors.in.Seed)] <- median(df$Number.of.Investors.in.Seed,na.rm = T) # fill in missing value of chosen variable with median
df$Age.of.company.in.years[is.na(df$Age.of.company.in.years)] <- median(df$Age.of.company.in.years,na.rm = T) # fill in missing value of chosen variable with median
df$Internet.Activity.Score[is.na(df$Internet.Activity.Score)] <- median(df$Internet.Activity.Score,na.rm = T) # fill in missing value of chosen variable with median
df$Team.size.all.employees[is.na(df$Team.size.all.employees)] <- median(df$Team.size.all.employees,na.rm = T) # fill in missing value of chosen variable with median
df$Last.Funding.Amount[is.na(df$Last.Funding.Amount)] <- median(df$Last.Funding.Amount,na.rm = T) # fill in missing value of chosen variable with median

# Outliers
for (i in c(8,19,20,21,22,23,24)) { # All numeric variable's number of outlier using interquartile method 
  q1=quantile(df[,i], .25)
  q3=quantile(df[,i], .75)
  IQR=IQR(df[,i])
  count_out<-subset(df, df[,i] > (q1 - 1.5*IQR) & df[,i] < (q3 + 1.5*IQR))
  print(paste0(colnames(df)[i]," variable count of outlier - ",count(df)-count(count_out)))
}
for (i in c(8,19,20,21,22,23,24)) {
  df[,i][df[,i] %in% boxplot(df[,i])$out] <- median(df[,i]) # Replace all outlier values by median
}


###-------------------------Variable selection and create variable------------------------- 
df$Dependent.Company.Status=ifelse(df$Dependent.Company.Status=='Success',1,0) # recoding target variable
df$Dependent.Company.Status<-as.integer(df$Dependent.Company.Status) # recoding target variable
df$Team.size.all.employees2=df$Team.size.all.employees^2 # create new variable for checking non-linear relationship
df$Age.of.company.in.years2=df$Age.of.company.in.years^2 # create new variable for checking non-linear relationship
par(mfrow=c(4,2))
hist(df[,8],main = paste(colnames(df)[8])) # histogram of numeric variable
hist(df[,19],main = paste(colnames(df)[19])) # histogram of numeric variable
hist(df[,20],main = paste(colnames(df)[20])) # histogram of numeric variable
hist(df[,21],main = paste(colnames(df)[21])) # histogram of numeric variable
hist(df[,22],main = paste(colnames(df)[22])) # histogram of numeric variable
hist(df[,23],main = paste(colnames(df)[23])) # histogram of numeric variable
hist(df[,24],main = paste(colnames(df)[24])) # histogram of numeric variable
par(mfrow=c(1,1)) # Decided to remove following variables Number.of.Investors.in.Seed, Number.of.of.advisors based on distribution of numerical variable
par(mfrow=c(4,5))
for (i in c(2,3,4,5,6,7,9,10,11,12,13,14,15,16,17,18,25,26,27)) { # Frequency distribution of categorical variables
  barplot(prop.table(table(df[,i])),main = colnames(df)[i]) 
}
par(mfrow=c(1,1)) # Decided to Remove following variables based on frequency distribution - Worked.in.top.companies, Was.he.or.she.partner.in.Big.5.consulting., "Speech.analytics.business","Number.of..of.Partners.of.company"
df=df %>% dplyr::select(-c("Number.of.Investors.in.Seed","Number.of.of.advisors","Worked.in.top.companies","Was.he.or.she.partner.in.Big.5.consulting.",
                    "Speech.analytics.business","Number.of..of.Partners.of.company"))

###-------------------------Data split into train and test-------------------------
df$Dependent.Company.Status<-as.factor(df$Dependent.Company.Status)
split <- createDataPartition(df$Dependent.Company.Status, p = 0.7,list = FALSE) # Divide data into train 70%, test 30%
df_train <- df[c(split),] # train set
df_test <- df[-c(split),] # validation set
df=read_rds("df.rds") # We saved cleaned data as df.rds
split=read_rds("split.RDS") # 
df_train=read_rds("df_train.rds") # we saved train set as df_train
df_test=read_rds('df_test.rds') # we saved test set as df_test
table(df_train$Dependent.Company.Status) # frequency of target variable in train set
table(df_test$Dependent.Company.Status) # frequency of target variable in test set

###-------------------------Data Recoding-------------------------
df_train$average_L=ifelse(df_train$Average.size.of.companies.worked.for.in.the.past=='Large',1,0) # create new explanatory dummy variable with binary choice
df_train$average_M=ifelse(df_train$Average.size.of.companies.worked.for.in.the.past=='Medium',1,0) # create new explanatory dummy variable with binary choice
df_train$average_S=ifelse(df_train$Average.size.of.companies.worked.for.in.the.past=='Small',1,0) # create new explanatory dummy variable with binary choice
df_train$struct_S=ifelse(df_train$Focus.on.structured.or.unstructured.data=='Structured',1,0) # create new explanatory dummy variable with binary choice
df_train$struct_U=ifelse(df_train$Focus.on.structured.or.unstructured.data=='Unstructured',1,0) # create new explanatory dummy variable with binary choice
df_train$struct_B=ifelse(df_train$Focus.on.structured.or.unstructured.data=='Both',1,0) # create new explanatory dummy variable with binary choice
df_train=df_train %>% dplyr::select(-c(Average.size.of.companies.worked.for.in.the.past,Focus.on.structured.or.unstructured.data)) 

df_test$average_L=ifelse(df_test$Average.size.of.companies.worked.for.in.the.past=='Large',1,0) # create new explanatory dummy variable with binary choice
df_test$average_M=ifelse(df_test$Average.size.of.companies.worked.for.in.the.past=='Medium',1,0) # create new explanatory dummy variable with binary choice
df_test$average_S=ifelse(df_test$Average.size.of.companies.worked.for.in.the.past=='Small',1,0) # create new explanatory dummy variable with binary choice
df_test$struct_S=ifelse(df_test$Focus.on.structured.or.unstructured.data=='Structured',1,0) # create new explanatory dummy variable with binary choice
df_test$struct_U=ifelse(df_test$Focus.on.structured.or.unstructured.data=='Unstructured',1,0) # create new explanatory dummy variable with binary choice
df_test$struct_B=ifelse(df_test$Focus.on.structured.or.unstructured.data=='Both',1,0) # create new explanatory dummy variable with binary choice
df_test=df_test %>% dplyr::select(-c(Average.size.of.companies.worked.for.in.the.past,Focus.on.structured.or.unstructured.data))

###-------------------------Descriptive analysis-------------------------
df_nums <-  
  sapply(df, is.numeric) %>% # Correlation between numeric variables
  which() %>% 
  names()
df_num_cor <- 
  cor(df[, df_nums],
      use = "pairwise.complete.obs")


###-------------------------Model-------------------------
#--------------------Logit, Probit
logit <- glm(Dependent.Company.Status~.,data=df_train,family=binomial(link="logit")) %>% # logit model with backward variable selection method
  stepAIC(trace = FALSE,direction = 'backward') # backward variable selection method
probit <- glm(Dependent.Company.Status~., data=df_train, # probit model with backward variable selection method
             family=binomial(link="probit")) %>% stepAIC(trace = FALSE,direction = 'backward')  # backward variable selection method
print(paste0(logit$aic,"  ",probit$aic)) # AIC - information criteria of logit and probit model

# logit, probit, lpm with the most important variables from backward variable selection method
logit <- glm(Dependent.Company.Status~Have.been.part.of.successful.startups.in.the.past.+Consulting.experience.+Age.of.company.in.years+
               Machine.Learning.based.business+Predictive.Analytics.business+Prescriptive.analytics.business+Big.Data.Business+Local.or.global.player+
               Internet.Activity.Score+Age.of.company.in.years2+average_L+average_M+struct_S+struct_U,data=df_train,family=binomial(link="logit")) # logit model with most important explanatory variables 
probit <- glm(Dependent.Company.Status~Have.been.part.of.successful.startups.in.the.past.+Consulting.experience.+Age.of.company.in.years+
                 Machine.Learning.based.business+Predictive.Analytics.business+Prescriptive.analytics.business+Big.Data.Business+Local.or.global.player+
                 Internet.Activity.Score+Age.of.company.in.years2+average_L+average_M+struct_S+struct_U,data=df_train,family=binomial(link="probit")) # probit model with most important explanatory variables
lpm = lm(as.numeric(Dependent.Company.Status)~Have.been.part.of.successful.startups.in.the.past.+Consulting.experience.+Age.of.company.in.years+
           Machine.Learning.based.business+Predictive.Analytics.business+Prescriptive.analytics.business+Big.Data.Business+Local.or.global.player+
           Internet.Activity.Score+Age.of.company.in.years2+average_L+average_M+struct_S+struct_U,data=df_train) # linear probability model with most important explanatory variables
summary(logit) # summary of logit model
summary(probit) # summary of probit model
summary(lpm) # summary of lpm model

# Breausch-pagan test
bptest(logit) # breusch-pagan test of logit model
bptest(probit) # breusch-pagan test of probit model
bptest(lpm) # breusch-pagan test of lpm model

# Marginal effects with average characteristics
(meff = logitmfx(Dependent.Company.Status~Have.been.part.of.successful.startups.in.the.past.+Consulting.experience.+Age.of.company.in.years+
                   Machine.Learning.based.business+Predictive.Analytics.business+Prescriptive.analytics.business+Big.Data.Business+Local.or.global.player+
                   Internet.Activity.Score+Age.of.company.in.years2+average_L+average_M+struct_S+struct_U, data = df_train, atmean=TRUE))

# R squared of logit model
PseudoR2(logit,which = c('all')) # all R squared
countR2<-function(m) mean(m$y==round(m$fitted.values)) 
countR2(logit) # count R squared of logit model
adj.countR2<-function(m) {
  n<-length(m$y)
  k<-max(table(m$y))
  correct<-table(m$y==round(m$fitted.values))[["TRUE"]]
  (correct-k)/(n-k)
}
adj.countR2(logit) # Adjusted count R squared of logit model

# LINKTEST
source("linktest.R")
linktest_result = linktest(logit)
summary(linktest_result)

# Likelihood Ratio test with 1 variables
logit_rest <- glm(Dependent.Company.Status~Have.been.part.of.successful.startups.in.the.past.+Consulting.experience.+Age.of.company.in.years+
                    Machine.Learning.based.business+Predictive.Analytics.business+Prescriptive.analytics.business+Big.Data.Business+Local.or.global.player+
                    Internet.Activity.Score+average_L+average_M+struct_S+struct_U,data=df_train,family=binomial(link="logit")) 
lrtest(logit, logit_rest)

# Likelihood Ratio test with all variables whole model
null_logit<-glm(Dependent.Company.Status~1,data=df_train,family=binomial(link="logit")) 
lrtest(logit, null_logit)



#--------------------KNN
ctrl_cv5 <- trainControl(method = "cv",number = 5) # cross validation
grid <- expand.grid(.k=seq(1,50,by=1)) # search space of hyperparameters
df_trainknn=df_train %>% dplyr::select(-c("Have.been.part.of.startups.in.the.past.","Focus.on.private.or.public.data.","Focus.on.consumer.data.",
                                   "Product.or.service.company.","Last.Funding.Amount","Number.of.Co.founders","Number.of..Sales.Support.material")) # data prepare for knn model
knn_train <- train(Dependent.Company.Status ~ ., data = df_trainknn,method = "knn", trControl = ctrl_cv5, tuneGrid = grid) # hyperparameter tuning
plot(knn_train$results$k, knn_train$results$Accuracy, ylab="",xlab="K", main="Accuracy") # optimal k
k<-knn_train$results$k[knn_train$results$Accuracy==max(knn_train$results$Accuracy)] # optimal k
k <- knn_train$bestTune #  optimal k
knn_model <- train(Dependent.Company.Status ~ ., data = df_trainknn, # train knn model with optimal k
        method = "knn", trControl = ctrl_cv5, tuneGrid = k)

#--------------------Performance metrics of logit and KNN model
# Predicted value
predict_logit_train <- logit %>% predict(df_train, type = "response") # predicted value of logit on train set
predict_logit_train <- as.factor(ifelse(predict_logit_train  > 0.5, 1, 0)) 
predict_logit_test <- logit %>% predict(df_test, type = "response") # predicted value of logit on test set
predict_logit_test <- as.factor(ifelse(predict_logit_test  > 0.5, 1, 0))
predict_knn_train<-predict(knn_model,df_train) # predicted value of KNN model on train set
predict_knn_test<-predict(knn_model,df_test) # predicted value of KNN model on test set
# Confusion matrix
confusionMatrix(predict_logit_train, df_train$Dependent.Company.Status) # performance matrix of predicted value of logit on train set
confusionMatrix(predict_logit_test, df_test$Dependent.Company.Status)# performance matrix of predicted value of logit on test set
confusionMatrix(predict_knn_train, df_train$Dependent.Company.Status)# performance matrix of predicted value of KNN on train set
confusionMatrix(predict_knn_test, df_test$Dependent.Company.Status)# performance matrix of predicted value of KNN on test set




