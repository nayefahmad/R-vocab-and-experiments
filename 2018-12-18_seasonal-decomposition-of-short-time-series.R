

#*****************************************************************
# SEASONAL DECOMPOSITION OF SHORT TIME SERIES (< 2 FULL YEARS)
# 2018-12-18
# Nayef 
#*****************************************************************

library(forecast)
library(fpp)
library(ggplot2)
library(tidyverse)
library(ggpubr)

# rm(list = ls())

# reference: https://robjhyndman.com/hyndsight/tslm-decomposition/ 


# 1) input dataset: ----------------
df <- ts(c(2735.869,2857.105,2725.971,2734.809,2761.314,2828.224,2830.284,
           2758.149,2774.943,2782.801,2861.970,2878.688,3049.229,3029.340,3099.041,
           3071.151,3075.576,3146.372,3005.671,3149.381),
         start=c(2016,8),
         frequency=12)

autoplot(df)


# 2) standard decomposition approaches (don't work): -----------
# try stl: 
stl(df)
# Error in stl(df) : series is not periodic or has less than two periods

# try decompose: 
decompose(df)
# Error in decompose(df) : time series has no or less than 2 periods



# 3) Fit models using tslm: --------------------

# > 3.1 model 1: trend only --------------
# ?tslm  # for fitting regression with time series components 

m1.decompose_df <- tslm(df ~ trend) 
summary(m1.decompose_df)

# note: this is really convenient! We can just just "trend" as a predictor! 
# you can also just use "season": e.g. fit.beer <- tslm(beer2 ~ trend + season)


# >> plot the trend and the orig time series: --------
p1.data.and.trend <- 
      data.frame(data = as.numeric(df), 
           trend = predict(m1.decompose_df), 
           period = 1:20) %>% 
      gather(key = "key", 
             value = "val", 
             -period) %>% 
      ggplot(aes(x = period, 
                 y = val, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      theme(legend.position = "none"); p1.data.and.trend



# > 3.2) model 2: approximate the seasonal pattern using Fourier terms -------

m2.fourier <- tslm(df ~ trend + fourier(df,2))
summary(m2.fourier)


# >> how does fourier( ) work? ------
?fourier
fourier(df, 2)  # %>% str
# K = 2 is the max order of the Fourier terms 
# by default, fourier( ) will return a series the same length as df 

# fourier returns a matrix containing terms from a Fourier series, up to order K

# The period of the Fourier terms is determined from the time series
# characteristics of x.

# When x is a ts object, the value of K should be an integer and specifies the
# number of sine and cosine terms to return. Thus, the matrix returned has 2*K
# columns.


# reference:
# https://otexts.org/fpp2/useful-predictors.html#fourier-series

# "With Fourier terms, we often need fewer predictors than
# with dummy variables, especially when m islarge. This
# makes them useful for weekly data, for example, where m ≈
# 52. For short seasonal periods (e.g., quarterly data),
# there is little advantage in using Fourier terms over
# seasonal dummy variables."


# >> plot the individual fourier terms: 
fourier(df, 2) %>% 
      as.data.frame() %>% 
      gather() %>% 
      mutate(period = rep(1:20, 4), 
             key = as.factor(key)) %>% 
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      facet_wrap(~key)


# what does the sum of all these terms look like? 
sum.of.fouriers <- fourier(df, 2) %>%
    as.data.frame() %>% 
    apply(MARGIN = 1, 
          FUN = sum)

# >> plot sum of fourier terms: 
p2.fourier.terms <- 
      data.frame(period = rep(1:20), 
                 value = sum.of.fouriers) %>% 
      ggplot(aes(x = period, 
                 y = value)) +
      geom_hline(yintercept = 0, 
                 col = "grey60") + 
      geom_line(col = "coral2"); p2.fourier.terms


# compare with the original series: 
ggarrange(p1.data.and.trend, 
          p2.fourier.terms, 
          nrow = 2)
# the fourier terms seem to do a pretty decent job of tracking the variation 
# from the trend line



# now let's add in the final trend + fourier series: 
p3.final.series <- 
      data.frame(data = as.numeric(df), 
                 predicted.with.fourier = predict(m2.fourier), 
                 period = 1:20) %>% 
      gather(key = "key", 
             value = "value", 
             -period) %>%  
      
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 col = key)) + 
      geom_line() + 
      theme(legend.position = "bottom"); p3.final.series


# 4) decomposition into trend/season/remainder: ------------




