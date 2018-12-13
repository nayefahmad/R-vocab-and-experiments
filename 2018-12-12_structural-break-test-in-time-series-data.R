

#****************************************************
# STRUCTURAL BREAK TEST IN TIME SERIES DATA 
# 2018-12-12
# Nayef 
#****************************************************

library(strucchange)
library(tidyverse)

# references: 
# > https://stats.stackexchange.com/questions/93529/dummies-instead-of-the-chow-test 


# 1) create fake data with breakpoint: --------
df1.fake.data <- 
    data.frame(x = seq(1, 100), 
               group = c(rep(0, 50), 
                         rep(1, 50))) %>% 
    mutate(y_orig = ifelse(group == 0, 
                           2*x,
                           2*x[51]),  # hold constant at twice the value of x[51]
           z = map_dbl(x, function(x){rnorm(1, 0, 2)}),  # fn actually doesn't take any args  
           y = y_orig + z)
    
# str(df1.fake.data)

# plot data: 
df1.fake.data %>% 
ggplot(aes(x = x, 
           y = y)) + 
    geom_point()



# 1.1) now let's test for structural change at x = 51 ----- 
?sctest
?sctest.default
?sctest.formula

# test for structural break at point 51: 
m1 <- lm(y ~ x, 
         data = df1.fake.data)

sctest(m1, 
       type = "Chow", 
       point = 51)
# f(efp) = 3.5183, p-value = 7.083e-11
# Low p-value suggests that there is a break 

# alternative specification: 
sctest(y ~ x, 
       data = df1.fake.data, 
       type = "Chow", 
       point = 51)
# F = 2883.4, p-value < 2.2e-16
# Low p-value suggests that there is a break 

#**********************************************
# What if we try on some other point? 
# sctest(m1,
#        type = "Chow", 
#        point = 80)

# umm ... this test seems to think every point is a 
# break point?? 
# todo: what's going on here? 

# Ans. apparently the test is meant to be for a 
# specified breakpoint only? 
# https://en.wikipedia.org/wiki/Structural_break#cite_note-12 
# For linear regression models, the Chow test is 
# often used to test for a single break in mean at a 
# known time period K for K ∈ [1,T].[3][4] This test 
# assesses whether the coefficients in a regression 
# model are the same for periods [1,2, ...,K] and 
# [K + 1, ...,T]
#**********************************************


# 2) Situation 2: no break in series -----
df2.no.break <- 
    data.frame(x = seq(1, 100)) %>% 
    mutate(y_orig = 2*x, 
           z = map_dbl(x, function(x){rnorm(1, 0, 2)}),  # fn actually doesn't take any args  
           y = y_orig + z)

str(df2.no.break)             

# plot: 
df2.no.break %>% 
    ggplot(aes(x=x, 
               y=y)) + 
    geom_point()

# 2.1) chow test: -------
m2 <- lm(y ~ x, 
         data = df2.no.break)

sctest(m2, 
       type = "Chow", 
       point = 51)
# f(efp) = 0.51053, p-value = 0.9981
# no breakpoint detected 

# alternative specification: 
sctest(y ~ x,
       data = df2.no.break, 
       type = "Chow", 
       point = 51)
# f(efp) = 0.51053, p-value = 0.9981
# no breakpoint detected 




# 3) fake data with breakpoint and other covariate: ------
df3.with.covariate <- 
    data.frame(x = seq(1, 100), 
               group = c(rep(0, 50), 
                         rep(1, 50)), 
               category = sample(c("A", "B"), 
                                   100, 
                                   replace = TRUE) %>% 
                   as.factor) %>% 
    mutate(y_orig = ifelse(group == 0, 
                           2*x,
                           2*x[51]),  # hold constant at twice the value of x[51]
           z = map_dbl(x, function(x){rnorm(1, 0, 2)}),  # fn actually doesn't take any args  
           y = y_orig + z)


str(df3.with.covariate)


# 3.1) chow test after controlling for category: ----
m3 <- lm(y ~ x + category,
         data = df3.with.covariate)

sctest(m3,
       type = "Chow", 
       point = 51)
# (efp) = 3.5007, p-value = 1.36e-10

# alternative specification: 
sctest(y ~ x + category, 
       data = df3.with.covariate, 
       type = "Chow", 
       point = 51)
# F = 2004.2, p-value < 2.2e-16
# yes, we still see the breakpoint



# 4) without breakpoint, but with other covariate: ------
df4.no.break.with.covariate <- 
    data.frame(x = seq(1, 100)) %>% 
    mutate(y_orig = 2*x, 
           z = map_dbl(x, function(x){rnorm(1, 0, 2)}),  # fn actually doesn't take any args  
           y = y_orig + z, 
           category = sample(c("A", "B"), 
                             100, 
                             replace = TRUE) %>% 
               as.factor)

str(df4.no.break.with.covariate)


# 4.1) chow test: -----
m4 <- lm(y ~ x + category, 
         data = df4.no.break.with.covariate)

sctest(m4, 
       type = "Chow", 
       point = 51) 
# f(efp) = 0.61884, p-value = 0.9958
# no breakpoint detected 

# alternative specification: 
sctest(y ~ x + category, 
       data = df4.no.break.with.covariate, 
       type = "Chow", 
       point = 51)
# F = 0.093457, p-value = 0.9635
# no breakpoint detected 
