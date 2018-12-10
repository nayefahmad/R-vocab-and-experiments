
#*************************************************
# USING MEANS TO ESTIMATE PERCENTILES 
# 2018-12-09 

# Reference: https://link.springer.com/article/10.1007/s10729-007-9033-8 
#*************************************************

library(tidyverse)
library(magrittr)
library(gghighlight)

# rm(list = ls())

# input data: -------------
# A set of exponential distributions rates

# Note: the kind of graph we're aiming for only makes 
# sense for single-parameter distributions, or a set 
# of multi-parameter distributions that vary only along 
# a single parameter. 

df1.exponentials <- 
    data.frame(rate = seq(.01, .05, .001)) %>%
    mutate(mean = 1/rate)


# create function for adding percentiles: 
perc_function <- function(rate, quantile.param = 0.5){
    # generate sample from exponential dist: 
    random.vars <- rexp(100000, rate)
    
    # return specified quantile: 
    return(quantile(random.vars, quantile.param) %>% 
               unname %>% unlist)
}


# test the function: 
# perc_function(.002, .05)
# perc_function(.002)


# add in the percentiles: 
df1.exponentials %<>% 
    mutate(perc5 = map_dbl(rate,  # 1st arg - 1 for each row in df1
                           perc_function,  # function to map 
                           quantile.param = 0.05),  # 2rd arg - fixed
                     
           perc25 = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.25),  
           
           perc50 = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.50),  
           
           perc75 = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.75),  
           
           perc95 = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.95)  
           )

# result: 
df1.exponentials
str(df1.exponentials)


# reshape input for plotting: --------
df2.reshaped <- 
    df1.exponentials %>% 
    gather(key = "key", 
           value = "value", 
           -c(rate, mean)) %>% 
    
    # set factor levels: 
    mutate(key = factor(key, 
                        levels = c("perc5", 
                                   "perc25",
                                   "perc50",
                                   "perc75",
                                   "perc95")))
str(df2.reshaped)

# create plot: --------
p1.ready.reckoner <- 
    df2.reshaped %>% 
    
    ggplot(aes(x = value, 
               y = mean, 
               group = key, 
               colour = key)) + 
    geom_line() + 
    gghighlight() + 
    
    labs(title = "Ready-reckoner", 
         subtitle = Sys.time()); p1.ready.reckoner







