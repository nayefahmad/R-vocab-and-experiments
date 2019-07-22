

#'--- 
#' title: "Confidence interval for a Poisson mean"
#' author: "Nayef Ahmad"
#' date: "2019-07-22"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' 

#+ lib, include = FALSE 
library(tidyverse)
library(kableExtra)


#' ## Data

#+ analysis 
df1.data <- 
  tibble::tribble(
    ~fiscal_year, ~site, ~discharges, ~in_hospital_deaths,
         "16/17", "VGH",        4288,                  32,
         "17/18", "VGH",        4476,                  31,
         "18/19", "VGH",        4450,                  39
    )


df1.data %>% 
  mutate(lambda_hat = in_hospital_deaths/discharges) %>%  
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))


#' ## Confidence interval for Poisson mean:
#' * **Using normal approx**: https://stats.stackexchange.com/questions/15371/how-to-calculate-a-confidence-level-for-a-poisson-distribution
#' * **Without normal approx**: http://ms.mcmaster.ca/peter/s743/poissonalpha.html
#'
#' 


#' ## Confidence interval for binomial proportion: 

#' See Brown, Cai, DasGupta (2001), Statistical Science
#'
#' The standard "textbook" CI for binomial proportion has poor coverage when the
#' actual proportion is close to 0 or 1
#' 






    
  


