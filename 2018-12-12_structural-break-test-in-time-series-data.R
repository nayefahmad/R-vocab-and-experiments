

#****************************************************
# STRUCTURAL BREAK TEST IN TIME SERIES DATA 
# 2018-12-12
# Nayef 
#****************************************************

library(strucchange)
library(tidyverse)

# references: 
# > https://stats.stackexchange.com/questions/93529/dummies-instead-of-the-chow-test 


# create fake data: --------
df1.fake.data <- 
    data.frame(x = seq(1, 100), 
               group = c(rep(0, 50), 
                         rep(1, 50))) %>% 
    mutate(y_orig = ifelse(group == 0, 
                           2*x,
                           2*x[51]),  # hold constant at twice the value of x[51]
           z = map_dbl(x, function(x){rnorm(1, 0, 5)}),  # fn actually doesn't take any args  
           y = y_orig + z, 
           period = 1:n())
    
# str(df1.fake.data)

# plot data: 
df1.fake.data %>% 
ggplot(aes(x = period, 
           y = y)) + 
    geom_point()







