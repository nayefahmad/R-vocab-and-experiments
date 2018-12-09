
#*************************************************
# USING MEANS TO ESTIMATE PERCENTILES 
# 2018-12-09 

# Reference: https://link.springer.com/article/10.1007/s10729-007-9033-8 
#*************************************************

library(tidyverse)
library(magrittr)

# input data: -------------
# A set of normal distributions means and variances 

df1.normals <- 
    data.frame(mean = seq(100, 150, 5), 
               var = seq(5, 15))


# create function for adding percentiles: 
perc_function <- function(mean, var, quantile.param = 0.5){
    # generate sample from normal dist: 
    random.vars <- rnorm(1000, mean, var)
    
    # return specified quantile: 
    return(quantile(random.vars, quantile.param) %>% 
               unname)
}


# test the function: 
perc_function(100, 12, .05)
perc_function(100, 12)


# add in the percentiles: 
df1.normals %<>% 
    mutate(perc5 = map2(mean,  # 1st arg - 1 for each row in df1
                        var,  # 2nd arg - 1 for each row in df1
                        perc_function,  # function to map 
                        quantile.param = 0.05),  # 3rd arg - fixed
                     
           perc25 = map2(mean,  # 1st arg - 1 for each row in df1
                         var,  # 2nd arg - 1 for each row in df1
                         perc_function,  # function to map 
                         quantile.param = 0.25),  # 3rd arg - fixed
           
           perc50 = map2(mean,  # 1st arg - 1 for each row in df1
                         var,  # 2nd arg - 1 for each row in df1
                         perc_function,  # function to map 
                         quantile.param = 0.50),  # 3rd arg - fixed
           
           perc75 = map2(mean,  # 1st arg - 1 for each row in df1
                         var,  # 2nd arg - 1 for each row in df1
                         perc_function,  # function to map 
                         quantile.param = 0.75),  # 3rd arg - fixed
           
           perc95 = map2(mean,  # 1st arg - 1 for each row in df1
                         var,  # 2nd arg - 1 for each row in df1
                         perc_function,  # function to map 
                         quantile.param = 0.95)  # 3rd arg - fixed
           )

# result: 
df1.normals




