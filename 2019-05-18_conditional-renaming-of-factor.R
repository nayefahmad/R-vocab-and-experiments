

#*****************************************************
# Conditional relabel a values in a factor column 
# 2019-05-18
# Nayef 
#*****************************************************

library(tidyverse)

# Data: -------------
set.seed(1)

df <- data.frame(fruit = sample(c("apple", 
                                  "banana", 
                                  "cherry"), 
                                10, 
                                replace = TRUE)) %>% 
    mutate(colour = case_when(fruit == "apple" ~ "red", 
                              fruit == "banana" ~ "yellow", 
                              fruit == "cherry" ~ "red") %>% 
               as.factor())

str(df)
head(df, 10)

# Problem statement: ------------
# Change colour to green when fruit is apple (but leave
# cherry as is). DON'T convert fruit to a char, then convert
# back. The point is to find a better way than that.



# Solution: ---------------

# help(package = "forcats")
# ?fct_recode
# ?fct_match
# ?replace

# This works, but isn't very elegant bcoz requires
# converting colour to character then back to factor: 
df <- df %>% 
    mutate(colour = case_when(fruit == "apple" ~ "green", 
                              TRUE ~ as.character(colour)) %>% factor)

str(df)
head(df, 10)


