

#************************************
# GGPLOT: LEGEND FOR MULTIPLE LINE PLOTS
# 2019-02-13
# Nayef 

# question: how to get a legend for the mutiple lines in ggplot? 
#************************************

library(tidyverse)
library(gapminder)


# 1. get example data: : ----------------------
df1.gapminder <- gapminder %>% 
      filter(country == "Canada")


# 2. first plot: no legend : ----------------------
ggplot(df1.gapminder, 
       aes(x = year, 
           y = lifeExp)) + 
    
    # separately create 3 lines: 
    geom_line() + 
    geom_hline(yintercept = 70, 
               col = "blue") + 
    geom_hline(yintercept = 80, 
               col = "red")



# question: how to get a legend for the mutiple lines in ggplot? 
# answer: restructure the data, so that the values for all 3 lines are in a
#   single column, which is a factor

# 3. restructure the data: ----------------------
df2.gapminder.restructured <- 
    df1.gapminder %>% 
    select(year,
           lifeExp) %>% 
    
    # create new columns with the y-axis value of the blue and red lines
    mutate(blue_line = rep(70, n()), 
           red_line = rep(80, n())) %>% 
    
    # now gather all three columns into a single column: 
    gather(key = "line", 
           value = "value", 
           -year) %>% 
    
    # convert to factor: 
    mutate(line = as.factor(line))

# result: 
df2.gapminder.restructured




# 4. now plot the restructured data, with legend: : ----------------------
df2.gapminder.restructured %>% 
    ggplot(aes(x = year, 
               y = value, 
               group = line)) +  # "group" argument is used to separate the three "levels" of the single column with y-axis values 
    
    # now we need only one call to geom_line: 
    geom_line(aes(colour = line)) + 
    
    scale_color_manual(values = c("blue",
                                  "black",
                                  "red"))


