

#'--- 
#' title: "Confidence interval for a binomial proportion"
#' author: "Nayef Ahmad"
#' date: "2019-07-22"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---
#' 
#' 

#+ lib, include = FALSE 
library(tidyverse)
library(kableExtra)
library(Hmisc)

#' ## Data
#' We're looking at deaths as a proportion of all hospital discharges. 
#' 
#' **Binomial experiment assumptions**: 
#' 
#' * Each discharge is a "trial" with 2 possible outcomes - death/no death
#' * Trials are independent (this may be questionable)
#' * Constant probability of "success"^[Yes, it's morbid, but a death is a "success" here] on each trial (this may be questionable) 

#+ analysis 
df1.data <- 
  tibble::tribble(
    ~fiscal_year, ~site, ~discharges, ~in_hospital_deaths,
         "16/17", "VGH",        4288,                  32,
         "17/18", "VGH",        4476,                  31,
         "18/19", "VGH",        4450,                  39
    )

df1.data <- 
  df1.data %>% 
  mutate(p_hat = in_hospital_deaths/discharges)  

df1.data %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))




#' ## Confidence interval for binomial proportion: 

#' See Brown, Cai, DasGupta (2001), Statistical Science
#'
#' The standard "textbook" CI for binomial proportion has poor coverage when the
#' actual proportion is close to 0 or 1
#' 
#' Better option: use the Wilson interval. 
#' 
#' *Reference*: http://math.furman.edu/~dcs/courses/math47/R/library/Hmisc/html/binconf.html 
#' 
#' "Following Agresti and Coull, the Wilson interval is to be preferred and so is the default."  

#'
#' ### Wilson interval (recommended)

binconf(32, 4288, method = "wilson")
# binconf(32, 4288, method = "wilson")[2]  # lower est 
# binconf(32, 4288, method = "wilson")[3]  # upper est

# is this a symmetric CI? No. 
# binconf(32, 4288, method = "wilson")[1] - binconf(32, 4288, method = "wilson")[2]
# binconf(32, 4288, method = "wilson")[3] - binconf(32, 4288, method = "wilson")[1]

#' ### "Standard" interval (not recommended)

# for comparison, using the "textbook interval"
binconf(32, 4288, method = "asymptotic")
# is this a symmetric CI? Yes. 
# binconf(32, 4288, method = "asymptotic")[1] - binconf(32, 4288, method = "asymptotic")[2]
# binconf(32, 4288, method = "asymptotic")[3] - binconf(32, 4288, method = "asymptotic")[1]



#' 
#' ## Updating the data 

df2.data_with_ci <-
  df1.data %>% 
  mutate(ci_lower = map2_dbl(in_hospital_deaths, 
                             discharges, 
                             function(x, y){
                               binconf(x, y, 
                                       method = "wilson")[2]
                               
                             }), 
         ci_upper = map2_dbl(in_hospital_deaths, 
                             discharges, 
                             function(x, y){
                               binconf(x, y, 
                                       method = "wilson")[3]
                               
                             }))

df2.data_with_ci %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
              "condensed", 
              "responsive"))
              


#' \  
#' \  
#' \  

#' 
#' ## Confidence interval for Poisson mean (todo):
#' * **Using normal approx**: https://stats.stackexchange.com/questions/15371/how-to-calculate-a-confidence-level-for-a-poisson-distribution
#' * **Without normal approx**: http://ms.mcmaster.ca/peter/s743/poissonalpha.html
#'
#' 

  


