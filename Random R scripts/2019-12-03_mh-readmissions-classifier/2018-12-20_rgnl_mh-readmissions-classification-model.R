

#*****************************************************
# MH READMISSIONS: Classification using Xgboost 
#*****************************************************

library(here)
library(tidyverse)
library(magrittr)
library(eeptools)
library(lubridate)
library(Matrix)
library(skimr)
library(caret)
library(xgboost)
library(pROC)
library(e1071)
library(DiagrammeR)

# rm(list = ls())


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
                   as.numeric) %>% 
      
      # select cols we want: 
      select(Gender, 
             denom_age, 
             LHAName, 
             PostalCode, 
             GPCode, 
             denom_Institution, 
             los_days, 
             AdmissionFacilityLongName, 
             denom_nursingUnitCode, 
             AdmissionNursingUnit, 
             AdmittingDrService, 
             AdmissionAttendingDoctorService, 
             AdmissionCategoryCode, 
             AdmissionCategoryGroup, 
             AdmissionPatientServiceDADDescription, 
             ED_visits_all_time:ED_admits_last_365days, 
             elective_admits_all_time:elective_admits_last_365_days, 
             num_schizophrenia_dx1_all_time, 
             Readmit_Flag) %>% 
      
      # convert all chars to factors: 
      mutate_if(is.character, factor)

str(df2.cleaned.data)
head(df2.cleaned.data)
summary(df2.cleaned.data)


# checking specific cols: 
skim(df2.cleaned.data)  # from package skimr 

# does skimr count both nulls and NAs as "missing"? 
# Ans. No. NULLs are just dropped completely! They don't even count as "missing"

apply(df2.cleaned.data, 2, function(x){sum(is.na(x))})  # GPCode has lost of NAs
apply(df2.cleaned.data, 2, function(x){sum(is.null(x))})  # no actual NULLs 

# lots of character "NULL"s 
apply(df2.cleaned.data, 2, 
      function(x){
            ifelse(x == "NULL", 1, 0) %>% 
                  sum(na.rm = TRUE)})



# recode NAs as "NULL": 
df2.cleaned.data$GPCode[is.na(df2.cleaned.data$GPCode)] <- "NULL"
# check NAs again: 
apply(df2.cleaned.data, 2, function(x){sum(is.na(x))})  # GPCode NAs recoded as "NULL" character


# check again: 
skim(df2.cleaned.data)




# 3) Train/test split: ----------------------------------------------
# todo: is a completely random split the best idea? Or should we use a "rolling
# forecasting origin" approach? (see
# http://topepo.github.io/caret/data-splitting.html#data-splitting-for-time-series)

set.seed(1)
train_index <- createDataPartition(df2.cleaned.data$Readmit_Flag, 
                                   p = 0.8,  # todo: should we change this? 
                                   list = FALSE, 
                                   times = 1)

df3.1.train.data <- df2.cleaned.data[train_index, ]  # training data 
df3.2.test.data <- df2.cleaned.data[-train_index, ]  # test data 



## > 3.1) set up training data predictors and response ----------
# try creating sparse matrix of predictors : 
m1.training.predictors <- sparse.model.matrix(Readmit_Flag ~ .,
                                              data = df3.1.train.data)[,-1]
# head(m1)

# vector of responses of the training data: 
train.target <- df3.1.train.data$Readmit_Flag


# join together in a DMatrix: 
train.dmatrix <- xgb.DMatrix(data = m1.training.predictors,
                             label = train.target)




## > 3.2) set up TEST data predictors and response ----------
m2.test.predictors <- sparse.model.matrix(Readmit_Flag ~ ., 
                                          data = df3.2.test.data)[,-1]
# head(m2.test.predictors)

# vector of responses for TEST data: 
test.target <- df3.2.test.data$Readmit_Flag




# 4) fit xgboost model: ------------------
mod1 <- xgboost(data = train.dmatrix, 
                nrounds = 300,  # todo: try changing this
                nthread = 2, 
                nfold = 5,  # todo: try changing this 
                metrics = "auc", 
                eta = 1,  # todo: try changing this 
                max_depth = 3,  # todo: try changing this 
                objective = "binary:logistic")



# > 4.1) cross-validation to estimate test error:---- 
mod.cv <- xgb.cv(data = train.dmatrix, 
               nrounds = 300, 
               nthread = 2, 
               nfold = 5, 
               metrics = "auc", 
               eta = 1, 
               max_depth = 3, 
               objective = "binary:logistic")

# looks like we can expect an AUC of around 0.63 - not good enough. Therefore,
# we should tune parameters/add predictors/do feature engineering


# > 4.2) ROC curve: -------
train.predicted.prob <- predict(mod1, m1.training.predictors)

roc1 <- roc(train.target,  # actual response values  
            train.predicted.prob, 
            algorithm = 1)  # predicted response values 

plot(roc1, col = "blue", 
     main = paste0("training set AUC = ",
                   auc(roc1) %>% round(2)))
auc(roc1)  # Area under the curve: 0.8381


# extract thresholds from the ROC curve: 
# ?coords
roc1.coords <- coords(roc = roc1, x = "all")  # todo: what's happening??
dim(roc1.coords)  # 3 30180
roc1.coords[1:3, 22000:22100]

coords(roc = roc1, x = .18)  # looks decent   
coords(roc = roc1, x = .19)  # looks decent   
coords(roc = roc1, x = .20)  # looks decent   

# > 4.3) confusion matrix: ----------
train.predicted.class <- as.numeric(train.predicted.prob > 0.5)  # todo: play with this
table(train.predicted.class)

confusionMatrix(factor(train.predicted.class), 
                factor(train.target))





# 5) interpret model built on training data: -----------
df4.mod1.imp <- xgb.importance(feature_names = colnames(m1.training.predictors), 
                               model = mod1)

head(df4.mod1.imp, 15)


xgb.plot.importance(head(df4.mod1.imp, 15))


# visualise the tree: 
xgb.plot.tree(feature_names = colnames(m1.training.predictors), 
              model = mod1, 
              trees = 0)

# save a lot of trees: 
list1.trees <- lapply(130:140, 
                      xgb.plot.tree, 
                      feature_names = colnames(m1.training.predictors),
                      model = mod1)
list1.trees

# export one graph 
# todo: doesn't work
export_graph(list1.trees[[1]],  
             here::here("results",
                        "code output",
                        "2018-12-20_xgboost-trees.pdf"))




# 6) test set performance: ------

# 6.1) roc: ---------------------
test.predicted.prob <- predict(mod1, m2.test.predictors)

roc2 <- roc(test.target,  # actual response values  
            test.predicted.prob, 
            algorithm = 1)  # predicted response values 

plot(roc2, 
     col = "blue", 
     main = paste0("test set AUC = ",
                   auc(roc2) %>% round(2)))

auc(roc2)  # Area under the curve: 0.6755


# > 6.2 confusion matrix: ------------
test.predicted.class <- as.numeric(test.predicted.prob > 0.6)
table(test.predicted.class)

confusionMatrix(factor(test.predicted.class), 
                factor(test.target))


