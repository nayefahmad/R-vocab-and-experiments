#*****************************************************
# MH READMISSIONS: CLEANING THE INPUT DATASET
#*****************************************************

library(tidyverse)
library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(ranger)       # a faster implementation of randomForest
library(caret)        # an aggregator package for performing many machine learning models
library(h2o)          # an extremely fast java-based platform
library(eeptools)
library(lubridate)
library(pROC)
library(ROCR)

rm(list = ls())

# 1) Read in the dataset --------
df1.input.data <- read_csv(here::here("results",
                                      "code output", 
                                      "2018-12-18_mh-readmissions-dataset-for-classification.csv"))
str(df1.input.data)


# 2) clean up the data: -------------------------------------------------
df2.cleaned.data <- 
  df1.input.data %>% 
  
  mutate(AdmissionAge = as.numeric(AdmissionAge), 
         denom_age = floor(age_calc(ymd(denom_BirthDate), 
                                    denom_AdmissionDate, 
                                    units = "years")), 
         los_days = difftime(denom_DischargeDate, 
                             denom_AdmissionDate, 
                             units = "days") %>% 
           as.numeric, 
         ED_visits_last_30to180days = ED_visits_last_180days - ED_visits_last_30days,
         ED_visits_last_180to365days = ED_visits_last_365days - ED_visits_last_180days,
         ED_admits_last_30daysto180days = ED_admits_last_180days - ED_admits_last_30days,
         ED_admits_last_180to365days = ED_admits_last_365days - ED_admits_last_180days, 
         elective_admits_last_30to180ays = elective_admits_last_180_days - elective_admits_last_30_days,
         elective_admits_last_180to365days = elective_admits_last_365_days - elective_admits_last_180_days) %>% 
  
  # select cols we want: 
  select(Gender, 
         denom_age, 
         denom_Institution, 
         los_days, 
         AdmissionCategoryCode, 
         # ED_visits_all_time,
         ED_visits_last_30days,
         ED_visits_last_30to180days,
         ED_visits_last_180to365days,
         # ED_admits_all_time,
         ED_admits_last_30days,
         ED_admits_last_30daysto180days,
         ED_admits_last_180to365days,
         # elective_admits_all_time,
         elective_admits_last_30_days,
         elective_admits_last_30to180ays,
         elective_admits_last_180to365days,
         Readmit_Flag) %>% 
  
  # convert all chars to factors: 
  mutate_if(is.character, factor)


# 3) Data Partition  -------
set.seed(123)
ind <- sample(2, nrow(df2.cleaned.data), replace=TRUE, prob = c(0.8, 0.2))
train <- df2.cleaned.data[ind==1,]
test <- df2.cleaned.data[ind==2,]


# 4) Fitting the Random Forest model -------
classifier <- randomForest(
  formula = as.factor(Readmit_Flag) ~ .,
  ntree = 300,
  mtry = 2,
  data = train,
  importance = TRUE) # notice the number of trees, number of splits and the confusion matrix
print(classifier)

# Error rate of Random Forest
plot(classifier) # Black solid line is for overall OOB error. Colour lines are for each classification error

# 5) Variable Importance -----
varImpPlot(classifier, sort = T, main = "Top Important Variables")
varUsed(classifier)

# 6) Prediction & Confusion Matrix -----
# A. train data
p1 <- predict(classifier, train)
confusionMatrix(p1, as.factor(train$Readmit_Flag))

# B. test data
p2 <- predict(classifier, test)
confusionMatrix(p2, as.factor(test$Readmit_Flag))


# 7) Tune Random Forest Model -----
# Tune mtry
train <- as.data.frame(train)
t <- tuneRF(train[,-15], 
            train[,15],
            stepFactor = 0.5,
            plot = TRUE,
            ntreeTry = 100,  
            trace = TRUE,   
            improve = 0.05) # Have the lowest OOB error when mtry = 2


# 8) Model Performance Evaluation -----
eval <- performance(rf_pr_test, "acc")
plot(eval)

# Identify best values of cutoff for accuracy
max <- which.max(slot(eval, "y.values")[[1]])
accuracy <- slot(eval, "y.values")[[1]][max]
cutoff <- slot(eval, "x.values")[[1]][max]
print(c(Accuracy=accuracy, Cutoff=cutoff)) # acc = 0.839, cutoff = 0.5867



# 9) ROC Curve ------
## A. Train set
rf_p_train <- predict(classifier, type = "prob", newdata = train)
rf_pr_train <- prediction(rf_p_train[,2], train$Readmit_Flag)
rf_perf_train <- performance(rf_pr_train, "tpr", "fpr")
plot(rf_perf_train, 
     colorize = T,
     main="train set ROC",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0, b=1)

# Area Under curve (AUC)
auc <- performance(rf_pr_train, "auc")
auc <- unlist(slot(auc, "y.values"))
legend(.6, .2, round(auc,4), title = "AUC")

## B. Test set
rf_p_test <- predict(classifier, type = "prob", newdata = test)
rf_pr_test <- prediction(rf_p_test[,2], test$Readmit_Flag)
rf_perf_test <- performance(rf_pr_test, "tpr", "fpr")
plot(rf_perf_test, 
     colorize = T,
     main="test set ROC",
     ylab = "Sensitivity",
     xlab = "1-Specificity")
abline(a=0, b=1)

# Area Under curve (AUC)
auc <- performance(rf_pr_test, "auc")
auc <- unlist(slot(auc, "y.values"))
legend(.6, .2, round(auc,4), title = "AUC")


