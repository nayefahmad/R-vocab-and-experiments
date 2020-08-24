
#********************************************************************
# MARS model test trive
# 2020-08-04
# Nayef 

# References: 
# http://uc-r.github.io/mars
# Applied Predictive Modeling, Ch 7 
#********************************************************************

library(tidyverse)
library(rsample)   # data splitting 
library(ggplot2)   # plotting
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
library(vip)       # variable importance
library(pdp)       # variable relationships
library(DBI)
library(dbplyr)
library(odbc)
library(broom)
library(lubridate)

# mtcars example ----
df1_cars <- mtcars

# fit mars 
m1_mtcars <- earth(mpg ~ disp, data = df1_cars)
summary(m1_mtcars)
plot(m1_mtcars)
names(m1_mtcars)

# wrangling for plot
df2_cars_predict <- 
  data.frame(mpg = df1_cars$mpg, 
             pred_mpg = predict(m1_mtcars) %>% unname, 
             disp = df1_cars$disp) %>% 
  pivot_longer(c(mpg, pred_mpg), 
               names_to = "var")

# plot
df2_cars_predict %>% 
  ggplot(aes(x = disp,
             y = value, 
             col = var)) + 
  geom_point() 



# Actual data ----------
# > db connection ----
cnx1 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SQL_D_16D1140")
kc135_tbf <- dplyr::tbl(cnx1, 
                        dbplyr::in_schema("anltx.dbo", 
                                          "KC135_time_between_failures"))

# > param ----
wuc1 <- "13EAE"

df3_wuc_selected <- 
  kc135_tbf %>% 
  dplyr::filter(wuc == wuc1) %>% 
  dplyr::select(-locations_icao) %>% 
  dplyr::collect() %>% 
  
  # for simplicity, start with small dataset 
  dplyr::mutate(days_from_2000 = difftime(ymd(end_date), 
                                          ymd("2000-01-01")) %>% 
                  as.double) %>%
  # dplyr::filter(days_from_2000 < 6500) %>%  # todo: what happens if we limit observations? 
  tidyr::drop_na()  # todo: is it okay to just throw out censored observations? 


# mars model 
m2 <- earth(flight_hours ~ days_from_2000, data = df3_wuc_selected)
summary(m2)

# wrangling for plot
df4_predict <- 
  data.frame(days_from_2000 = df3_wuc_selected$days_from_2000, 
             fh = df3_wuc_selected$flight_hours, 
             pred_fh = predict(m2) %>% unname) %>% 
  
  pivot_longer(c(fh, pred_fh), 
               names_to = "var") %>% 
  purrr::set_names(c("days_from_2000", 
                     "variable", 
                     "value"))
  
df4_predict %>% 
  ggplot(aes(x = days_from_2000,
             y = value,
             col = variable)) +
  geom_point() + 
  labs(title = str_glue(wuc1))



# Notes ----
# 1) As new data comes in, there's no guarantee that changepoints identified 
#  last time will be the same as this time (?). Is this a problem? I kind of 
#  don't think so - just use the latest ones, as they account for the fullest 
#  picture of things. But worth thinking about. 

# 2) What does this look like for the user? I think the graph is useful, as 
#  well as a simple binary alert: any changepoint in last 6 months? 
