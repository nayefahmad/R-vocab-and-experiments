
#****************************************************************
# Producing multiple plots using purrr::map() on a nested dataframe 
# 2019-02-12 
# Nayef  

# Reference: https://stackoverflow.com/questions/47482288/how-to-use-purrrpmap-to-plot-multiple-ggplot-in-nested-data-frame 

#****************************************************************



library(magrittr)
library(tidyverse)

iris0 <- iris

iris0 <- iris0 %>%
      group_by(Species) %>%  
      nest() %>%
      
      mutate(gg1 = purrr::map(data,
                              ~ ggplot(., aes(Sepal.Length, Sepal.Width)) + 
                                    geom_point())) 

# Note: the formula notation is alternative way of defining an anonymous
# function

# Instead of 
# > "~ ggplot(., aes(Sepal.Length, Sepal.Width))"

# we could write: 
# > "function(x){ggplot(x, aes(Sepal.Length, Sepal.Width))}


# Accessing/printing the list of plots: 
iris0$gg1

str(iris0$gg1)
