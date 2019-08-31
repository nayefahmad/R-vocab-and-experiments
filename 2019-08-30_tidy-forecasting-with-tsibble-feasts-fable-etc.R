
#'--- 
#' title: "Tidy forecasting with tsibble, feasts, fable, etc."
#' author: "Nayef Ahmad"
#' date: "2019-08-30"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#' References: 
#' 
#' * https://tidyverts.org/ 
#' * https://github.com/business-science/sweep
#' 



#+ lib, include = FALSE 
library(magrittr)
library(ggplot2)
library(feasts)
# library(fable)  todo: FAILs to install

#+ rest 
#' To begin, we'll simulate a random walk, then plot and analyze it using
#' `tsibble`, `feasts`, and `fable` packages
#' 

# function for a random walk: ----------
random_walk <- function(t = 100,
                        drift = 0,
                        sd_white_noise = .5){
  
  # Note that rnorm returns a vector
  return(cumsum(drift + rnorm(t, mean = 0, sd = sd_white_noise)))
  
}


#+ 
# simulate the random walk: ---------
df1 <- data.frame(time = 1:100, 
                  value = random_walk())

ts1 <- df1 %>% as_tsibble(index = time)
autoplot(ts1)


#' Can STL detect that there's no seasonality? 
#' 
#' Yes! 

# decomposition: --------------
STL(ts1) %>% autoplot()



#' Now let's fit a model 
#' 

# model fitting: ---------

ts1 %>% 
  model(RW(value ~ drift()))
  










