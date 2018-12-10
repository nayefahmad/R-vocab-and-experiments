
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
    mutate(`5th percentile` = map_dbl(rate,  # 1st arg - 1 for each row in df1
                           perc_function,  # function to map 
                           quantile.param = 0.05),  # 2rd arg - fixed
                     
           `25th percentile` = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.25),  
           
           `50th percentile` = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.50),  
           
           `75th percentile` = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.75),  
           
           `95th percentile` = map_dbl(rate,  
                            perc_function,  
                            quantile.param = 0.95)  
           )

# result: 
# df1.exponentials
# str(df1.exponentials)


# reshape input for plotting: --------
df2.reshaped <- 
    df1.exponentials %>% 
    gather(key = "key", 
           value = "value", 
           -c(rate, mean)) %>% 
    
    # set factor levels: 
    mutate(key = factor(key, 
                        levels = c("5th percentile", 
                                   "25th percentile",
                                   "50th percentile",
                                   "75th percentile",
                                   "95th percentile")))
# str(df2.reshaped)

# create plot: --------
p1.ready.reckoner <- 
    df2.reshaped %>% 
    
    ggplot(aes(x = value, 
               y = mean, 
               group = key, 
               colour = key)) + 
    
    geom_line() + 
    gghighlight(label_params = list(segment.colour = "grey60",
                                    segment.size = .1)) + 
    
    scale_y_continuous(limits = c(20, 120), 
                       breaks = seq(20, 120, 20)) +
    
    labs(title = "ED service times: calculating percentiles from observed averages", 
         subtitle = "Assuming service time is exponentially distributed, we can use the observed mean to infer the full \nprobability distribution \n", 
         y = "Observerd mean service time (minutes)", 
         x = "Service time associated with given percentile (minutes)", 
         caption  = "\n\nNote: Plot is based on simulated data") + 
    
    theme_classic(base_size = 12) + 
    theme(plot.caption = element_text(hjust = -0, 
                                      size = 8)); p1.ready.reckoner

# save plot: 
ggsave("2018-12-09_exponential-percentiles-from-means.jpeg", 
       width = 8)






