



library(tidyverse)

mtcars %>% 
    ggplot(aes(cyl, qsec)) + 
    
    # stat_summary with arg "fun.y":
    # A function that returns a single number 
    stat_summary(fun.y = mean, 
                 geom = "bar") + 
    
    # stat_summary with arg "fun.data": 
    # A function that is given the complete data and should return a data frame
    # with variables ymin, y, and ymax (for use in plotting ranges).
    
    # mean_se( ) is intended for use with stat_summary. It calculates mean and 
    # standard error 
    stat_summary(fun.data = mean_se,  
                 geom = "errorbar")

?stat_summary

