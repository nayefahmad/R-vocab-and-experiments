

#'--- 
#' title: "Regression residuals vs predictors"
#' author: "Nayef Ahmad"
#' date: "2019-07-03"
#' output: 
#'   html_document: 
#'     code_folding: hide
#' ---
#' 


#' Reference: https://twitter.com/alexpghayes/status/1033824995905077250?s=09 
#' 
#' 

library(tidyverse)
library(broom)
library(DT)

# fit regression: 
m1 <- lm(hp ~ ., 
         data = mtcars)


summary(m1)


# get residuals and fitted values: 
df1 <- augment(m1)

df1 %>% 
  datatable() %>% 
  formatRound(12:99, 2)



# gather predictors: 
df2 <- 
  df1 %>% 
  gather(key = "predictor", 
         value = "value", 
         -contains("."))  # contains() used to select vars with "." in name 

df2 %>% 
  datatable() %>% 
  formatRound(2:99, 2)



# resids vs predictors: 
df2 %>% 
  ggplot(aes(x = value, 
             y = .resid)) + 
  geom_point() + 
  geom_smooth() + 
  facet_wrap(~predictor, 
             scales = "free")  # x-axis for some predictors is small (e.g. `am`), but 
                               # for others it's large (e.g. `disp`). So we need 
                               # to set scales free 
