

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
         -contains("."))

df2 %>% 
  datatable() %>% 
  formatRound(2:99, 2)



# resids vs predictors: 
df2 %>% 
  ggplot(aes(x = value, 
             y = .resid)) + 
  geom_point() + 
  facet_wrap(~predictor, 
             scales = "free")
