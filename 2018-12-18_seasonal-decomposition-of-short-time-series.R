

#*****************************************************************
# SEASONAL DECOMPOSITION OF SHORT TIME SERIES (< 2 FULL YEARS)
# 2018-12-18
# Nayef 
#*****************************************************************

library(forecast)
library(fpp)
library(ggplot2)
library(tidyverse)

# reference: https://robjhyndman.com/hyndsight/tslm-decomposition/ 


# input dataset: ----------------
df <- ts(c(2735.869,2857.105,2725.971,2734.809,2761.314,2828.224,2830.284,
           2758.149,2774.943,2782.801,2861.970,2878.688,3049.229,3029.340,3099.041,
           3071.151,3075.576,3146.372,3005.671,3149.381),
         start=c(2016,8),
         frequency=12)

autoplot(df)


# standard decomposition approaches: -----------
# try stl: 
stl(df)
# Error in stl(df) : series is not periodic or has less than two periods

# try decompose: 
decompose(df)
# Error in decompose(df) : time series has no or less than 2 periods



# Fit models using tslm: --------------------

# > model 1: --------------
# ?tslm  # for fitting regression with time series components 

m1.decompose_df <- tslm(df ~ trend) 
summary(m1.decompose_df)

# note: this is really convenient! We can just just "trend" as a predictor! 

# >> plot the trend and the orig time series: --------
# todo: ggplot this
predict(m1.decompose_df) %>% plot()
lines(as.numeric(df))


# > model 2: approximate the seasonal pattern using Fourier terms -------

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

# plot the fourier terms: 
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
sum <- fourier(df, 2) %>% 
      as.data.frame() %>% 
      apply(MARGIN = 1, 
            FUN = sum)

data.frame(period = rep(1:20), 
           value = sum) %>% 
      ggplot(aes(x = period, 
                 y = value)) + 
      geom_line()

# compare with the original series: 
predict(m1.decompose_df) %>% plot()
lines(as.numeric(df))
