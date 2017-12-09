

#****************************
# VGH ED WINTER SURGE PLANNING 
#****************************

library("dplyr")
library("lubridate")

# rm(list=ls())
# getwd()

#****************************
#TODO:------------
# > see line 53 (defining year2017 dataframe )
# > check resids of regressions 
#****************************


#****************************
# import cleaned data: -----------
source("G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/src/2017-10-26_import-clean-ed-data.R")

# > split data by year, month  -----------
split.fyear <- split(visits, visits$fyear)
# str(split.fyear)

split.fyear.month <- split(visits, visits$month)
# str(split.fyear.month)


#****************************
# import function for regressions: ----------
source("G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/src/regress-num-ed-visits_function.R")

# test the function: 
# regress.fn(split.fyear[1])

# regressions by year: ------------
year.models <- lapply(split.fyear, regress.fn, summary=FALSE) # %>% print
year.models.summaries <- lapply(split.fyear, regress.fn) %>% print
# intercept is a Friday in December
year.models.coeffs <- lapply(year.models.summaries, coefficients)




#****************************
# get predicted values without taking trend into account: ------
year2017 <- data.frame(date=seq(ymd("2017-10-01"), ymd("2018-02-28"), by="day")) %>% 
      mutate(day.of.week=as.factor(weekdays(date)), 
             month=month(date, label=TRUE, abbr=TRUE), 
             fyear=as.factor(rep("16/17", 151)),  # todo: I'm forced to set 
                                                  # this as 16/17, even though 
                                                  # it's 17/18. How to fix this? 
             month.fyear=month(date, label=TRUE, abbr=TRUE) %>%
                   paste(fyear, sep="-") %>%
                   as.factor)

# str(year2017)


predict2017.no.trend <- predict(year.models[[7]], 
                                newdata=year2017) %>% as.data.frame 
predict2017.no.trend <- cbind(year2017, predict2017.no.trend) # %>% print 
names(predict2017.no.trend) <- c("date",
                                 "day.of.week",
                                 "month", 
                                 "fyear", 
                                 "month.fyear", 
                                 "num.visits")

predict2017.no.trend <- select(predict2017.no.trend, 
                               date,
                               day.of.week, 
                               month, 
                               num.visits) %>% print 



# summarized version: 
predict2017.no.trend.summary <- group_by(predict2017.no.trend, day.of.week, month) %>% 
      summarize(average=mean(num.visits)) %>% as.data.frame %>% print



# ********************************
# model2: year-on-year trends: -----------------
m2.regress.on.year <- lm(num.visits~fyear, data=visits) 
summary(m2.regress.on.year)

m2.regress.on.year.coeffs <- summary(m2.regress.on.year)$coefficients 




# ********************************
# write results to csv: ---------------
# > write reformatted raw data to csv: -----------
write.csv(visits, "G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/results/output from src/2017-10-24_vgh-ed-visits-formatted.csv", 
          row.names = FALSE)

# > write regression results to csv: ------------
setwd("G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/results/output from src")

lapply(year.models.coeffs,
       function(x) {write.table(data.frame(x),
                                '2017-10-25_regression-on-day-month-coefficients.csv'  ,
                                append= T,
                                sep=','
                                # row.names = FALSE
       )
       })

# > write predicted values -------
write.csv(predict2017.no.trend, "G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/results/output from src/2017-10-24_predict-2017-no-trend.csv", 
          row.names = FALSE)

# > write summarized predicted values -------
write.csv(predict2017.no.trend.summary, "G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/results/output from src/2017-10-24_predict-2017-no-trend-summary.csv", 
          row.names = FALSE)

# > write regression on year results -----------
write.csv(m2.regress.on.year.coeffs, "G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/results/output from src/2017-10-24_regression-on-year-coeffs.csv")


