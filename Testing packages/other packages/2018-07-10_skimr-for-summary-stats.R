

#*************************************************
# TESTING PACKAGE SKIMR 
#*************************************************

library("skimr")
library("MASS")
library("ggplot2")
library("magrittr")


skim(mtcars)
skim(iris)
skim(Boston)
skim(diamonds)

# well this is pretty cool :) 

# working with tidyverse: 
summary1 <- skim(diamonds)
str(summary1)  # not exactly sure what kind of object this is. 

summary1 %>% select(variable)  # doesn't work 
