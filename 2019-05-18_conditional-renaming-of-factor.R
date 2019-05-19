

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



# Solution 1: ---------------

# help(package = "forcats")
# ?fct_recode
# ?fct_match
# ?fct_expand
# ?replace

# This works, but isn't very elegant bcoz requires
# converting colour to character then back to factor: 
df <- df %>%
    mutate(colour = case_when(fruit == "apple" ~ "green",
                              TRUE ~ as.character(colour)) %>% factor)

str(df)
head(df, 10)



# Solution 2: ----------------
# df <- df %>% 
#     mutate(colour = fct_expand(df$colour, "green"))
# 
# levels(df$colour)

# This works as expected. Since we don't specify what to do
# when fruit != apple, we just get NAs. However, we do
# get a factor, as desired.
# df <- df %>% 
#     mutate(colour = case_when(fruit == "apple" ~ 
#                                   fct_recode(colour, 
#                                              green = "red")))
# 
# str(df)
# df
 

# So why does it all break down when we add the last
# condition to the case_when()? 

df <- df %>% 
    mutate(colour = case_when(fruit == "apple" ~ 
                                  fct_recode(colour, 
                                             green = "red"), 
                              TRUE ~ colour))

str(df)
df
