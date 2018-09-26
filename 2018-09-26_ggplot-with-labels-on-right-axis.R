

#**********************************************************
# GGPLOT WITH LINE LABELS ON RIGHT AXIS 
#**********************************************************

library("tidyverse")

# Reference: https://drsimonj.svbtle.com/label-line-ends-in-time-series-with-ggplot2 


# Orange dataset is preloaded
?Orange

df1.oranges <- Orange 

df1.oranges %<>%       
      select_all(tolower)  # select_all( ) takes a function as arg 

str(df1.oranges)
summary(df1.oranges)
head(df1.oranges)



# create a vector of the last (furtherst right) y-axis values for each group
final.ages <- df1.oranges %>% 
      group_by(tree) %>% 
      top_n(1, age) %>%  
      # top_n is a convenient wrapper that uses filter() and min_rank() to 
      #     select the top or bottom entries in each group, ordered by age 
      
      # Each tree has a set of age values. This orders each row by decreasing age values, 
      #     then picks the last value
      
      
      # select(circumference)
      pull(circumference)  # compare with select( ), which would not return a vector

final.ages
