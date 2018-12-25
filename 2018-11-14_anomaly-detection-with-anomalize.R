

#**************************************************************
# ANOMALY DETECTION IN TIME SERIES DATA USING "ANOMALIZE" PACKAGE 
#**************************************************************

library(gtrendsR)
library(tidyverse)
library(anomalize)

# reference: 
# https://github.com/cattystats/Anomaly_Detection
# https://business-science.github.io/anomalize/articles/anomalize_methods.html
# https://business-science.github.io/anomalize/index.html


# Example data: create df with google trends data
google_trends_df <-  gtrends(
  c("gingerbread"), #keywords -- start with one
  gprop = "web", #choose: web, news, images, froogle, youtube
  geo = c("CA"), #only pull results for BC, Canada 
  time = "2004-01-01 2018-11-30")[[1]] #timeframe


# str(google_trends_df)
# summary(google_trends_df)

#visualize with ggplot (optional but useful if you're choosing between keywords)
ggplot(data=google_trends_df, 
       aes(x=date, y=hits, group=keyword, col=keyword)) +
  geom_line() + 
  theme_classic() +
  labs(title = "Google Trends Data", 
       subtitle="CA search volume for 'eggnog'", 
       x="Time", y="Relative Interest") 



#prepare data for use in anomalize functions
google_trends_df_tbl <- google_trends_df %>%
  # mutate(date=lubridate::ymd(date)) %>%
  tbl_df()



# Next: Anomalize! 
# explore different methods for decomposition and anomaly detection
# choose the method that is best suited for the data you're analyzing

# twitter + gesd is generally better for highly seasonal data
# stl + iqr if seasonality is not a major factor
# adjust the trend period using domain knowledge about your data


# Method 1: STL + IQR Anomaly Detection
google_trends_df_tbl %>%   
  time_decompose(hits,
                 method = "stl",
                 trend = "1 year") %>%  # todo: what setting to use? 
  
  anomalize(remainder,
            method = "iqr") %>%
  time_recompose() %>%
  
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE) +
  labs(title = "Anomaly detection in Google searches for 'gingerbread' in Canada",
       x="Time",
       y="Relative Interest", 
       subtitle = "Other than Dec 1st and Nov 1st each year since 2004, the only outlier is Aug 1st, 2011. \nWonder what happened then? \n\n")




# Method 2: Twitter + IQR Anomaly Detection
google_trends_df_tbl %>%   
  time_decompose(hits,
                 method = "twitter",
                 trend = "1 year"
  ) %>%
  anomalize(remainder, method = "iqr") %>%
  time_recompose() %>%
      
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE) +
  labs(title = "Google Trends Data - Twitter + IQR Method",
       x="Time",
       y="Relative Interest", 
       subtitle = "CA search volume for 'gingerbread' between Jan'04-Nov'18"
  )



# Method 3: Twitter and GESD
google_trends_df_tbl %>%   
  time_decompose(hits,
                 method = "twitter",
                 trend = "1 year") %>%
  anomalize(remainder, method = "gesd") %>%
  time_recompose() %>%
  
  # Anomaly Visualization
  plot_anomalies(time_recomposed = TRUE) +
      
  # scale_x_date(breaks = c("2005-01-01", 
  #                         "2006-01-01") %>% as.Date) + 
       
  labs(title = "Google Trends Data - Twitter + GESD Method",
       x="Time",
       y="Relative Interest",
       subtitle = "CA search volume for 'gingerbread' between Jan'04-Nov'18"
  )



# look at how anomaly detection algorithm works
google_trends_df_tbl %>% 
  time_decompose(hits, method = "stl", 
                 frequency = "auto",
                 trend = "auto") %>%
  anomalize(remainder, 
            method = "iqr", 
            alpha = 0.04,
            max_anoms = 0.2) %>%
  plot_anomaly_decomposition() 
