

#**************************************************
# Gathering and Spreading using Stocks example 
# 2019-03-19
# Nayef 

#**************************************************

library(tidyverse)

# rm(list = ls())

?gather


# 1. Start with some non-tidy data: ------------
# > Recall, tidy data has only 1 observation per row 

stocks <- tibble(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
) %>% print


# 2. Gather into a tidy format, arranged by time: ---------
stocks_long_by_time <- 
    stocks %>% 
    gather(key = stock, 
           value = price, 
           -time) %>% print


# > 2.1 Notes: --------
# The var which is dropped with the minus sign (here, "time") will be
# repeated across rows in the resulting long format

# "time" variable is unique across rows in the original data

# If you don't drop "time" when spreading, you get weird results: 
# stocks %>% gather(key = stock, value = price)


# 3. Spread into a different non-tidy format: ------
stocks_wide <- 
    stocks_long_by_time %>% 
    spread(key = time, 
           value = price) %>% print

# > 3.1 Spread back into the original format: -----
stocks_long_by_time %>% 
    spread(key = stock, 
           value = price)


# 4. Gather again, arranging by stock: -----
stocks_long_by_stock <-  
    stocks_wide %>% 
    gather(key = time, 
           value = price, 
           -stock) %>% print

# again, note that the dropped variable, "stock", will be repeated across rows
# in the resulting long format. Also, it was unique across rows in the wide format. 


stocks_long_by_stock
stocks_long_by_time


    



