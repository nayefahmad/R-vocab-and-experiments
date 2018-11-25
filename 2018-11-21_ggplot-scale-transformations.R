

library(gapminder)
library(dplyr)
library(ggplot2)


gapminder %>% 
    ggplot(aes(x = year, 
               y = gdpPercap, 
               group = continent, 
               colour = continent)) + 
    geom_point() + 
    
    # different ways of doing transforms
    
    # option 1: 
    # scale_y_log10() + 
    
    # option 2: 
    coord_trans(y = "log10") + 
    
    geom_smooth()
