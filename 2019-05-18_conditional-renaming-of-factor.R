

#*****************************************************
# Conditional relabel a values in a factor column 
# 2019-05-18
# Nayef 
#*****************************************************

library(tidyverse)

# Data: -------------
df <- data.frame(fruit = sample(c("apple", 
                                  "banana"), 
                                10, 
                                replace = TRUE)) %>% 
    mutate(colour = case_when(fruit == "apple" ~ "red", 
                              fruit == "banana" ~ "yellow") %>% 
               as.factor())

str(df)


# Problem statement: ------------
# Change colour to green when fruit is apple. Don't convert
# fruit to a char, then convert back. The point is to find a
# better way than that.

