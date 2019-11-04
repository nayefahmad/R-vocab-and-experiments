
#'--- 
#' title: "Tidy forecasting with tsibble, feasts, fable, etc."
#' author: "Nayef Ahmad"
#' date: "2019-08-30"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#' References: 
#' 
#' * https://tidyverts.org/ 
#' * https://github.com/business-science/sweep
#' * model fitting: https://fable.tidyverts.org/articles/fable.html



#+ lib, include = FALSE 
library(magrittr)
library(ggplot2)
library(tsibble)
library(tsibbledata)  # for example data 
library(feasts)
library(fable)
library(DT)
library(lubridate)
library(denodoExtractor)
library(dbplyr)

#+ rest 
# example from fable website: ----------

aus_retail %>% str(max.level = 1)
aus_retail %>% summary

#' Top 1000 rows: 
aus_retail %>% 
  head(1000) %>% 
  datatable(extensions = 'Buttons',
            options = list(dom = 'Bfrtip', 
                           buttons = c('excel', "csv")))

#' Note that the data is in tidy format, and this makes it easy to produce 2
#' forecasts simultaneously, for 2 seperate levels of the variable `State`.
#'
#' Also note that in the final part of the pipe, you have the pass the data back
#' to `autoplot` to show both the historical data and the forecast
aus_retail %>%
  dplyr::filter(State %in% c("New South Wales", "Victoria"),
                Industry == "Department stores") %>% 
  
  fabletools::model(ets = ETS(box_cox(Turnover, 0.3)),
                    arima = ARIMA(log(Turnover)),
                    snaive = SNAIVE(Turnover)) %>% 
  
  fabletools::forecast(h = "2 years") %>% 
  autoplot(dplyr::filter(aus_retail, year(Month) > 2014))



#+ 
#' Let's simulate a random walk and try forecasting

# function for a random walk: ----------
random_walk <- function(t = 100,
                        drift = 0,
                        sd_white_noise = .5){
  
  # rnorm(t) generates all t random steps at once 
  # cumsum() sums it all up with the drift parameter at each step, 
  #   to find position at any point in time 
  
  return(cumsum(drift + rnorm(t, mean = 0, sd = sd_white_noise)))
  
}


# test: 
# random_walk(drift = .01)

#+ 
#' Next we'll simulate a random walk, then plot and analyze it using
#' `tsibble`, `feasts`, and `fable` packages
#' 

# simulate a random walk: ---------
set.seed(1)
df1 <- data.frame(time = 1:100, 
                  value = random_walk(drift = .01))

ts1 <- df1 %>% as_tsibble(index = time)
autoplot(ts1)


#' Can STL detect that there's no seasonality? 
#' 
#' Yes! 

# decomposition: --------------
STL(ts1) %>% autoplot()



#' Now let's fit a model 
#' 

# model fitting and forecast: ---------

ts1 %>% 
  model(RW(value ~ drift())) %>% 
  forecast(h = 10) %>% 
  autoplot(ts1) 
  



 




# admits forecasting: --------
# setup_denodo()
# 
# df2.lgh_admits <-
#   vw_adtc %>%
#   dplyr::filter(facility_name == "LGH Lions Gate",
#                 admit_date_id >= "20170101",
#                 admit_date_id < "20190901") %>%
#   dplyr::select(admit_date_id,
#                 nursing_unit_desc_at_ad) %>%
#   dplyr::count(admit_date_id,
#                nursing_unit_desc_at_ad) %>%
#   dplyr::collect()
# 
# 
# df2.lgh_admits %>%
#   dplyr::ungroup() %>%
#   dplyr::filter(nursing_unit_desc_at_ad %in% c("LGH 6 East",
#                                                "LGH 6 West",
#                                                "LGH 6 Surgical Close Observation")) %>%
#   fill_dates(admit_date_id,
#              "2017-01-01",
#              "2019-09-01") %>%
# 
#   dplyr::arrange(admit_date_id) %>%
#   dplyr::mutate(row_number = 1:dplyr::n(), 
#                 admit_date = ymd(admit_date_id)) %>% 
#   
#   tidyr::drop_na() %>% 
#   
#   as_tsibble(index = admit_date, 
#              key = nursing_unit_desc_at_ad) %>% 
# 
#   fabletools::model(ets_model = RW(n)) %>%
#   fabletools::forecast(h = "10 months") %>%
#   autoplot()
  


