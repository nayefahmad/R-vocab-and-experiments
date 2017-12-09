
#******************************
# IMPORT AND CLEAN ED DATA   
#******************************

library("dplyr")
library("lubridate")

# rm(list=ls())
# getwd()

# read data from csv file: ---------
visits <- read.csv("G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.25 VGH winter surge planning/data/2017-10-24_vgh-ed-visits.csv", 
                   stringsAsFactors = FALSE, 
                   header = TRUE)


# data wrangling: ---------------
visits <- mutate(visits, 
                 StartDate=ymd(StartDate), 
                 StartdateFiscalPeriodLong = as.factor(StartdateFiscalPeriodLong), 
                 StartDateFiscalYear=as.factor(StartDateFiscalYear)) %>% 
      rename(date=StartDate,
             fperiod=StartdateFiscalPeriodLong, 
             fyear=StartDateFiscalYear) %>%
      mutate(day.of.week=weekdays(date), 
             isweekend= ifelse (day.of.week %in% c("Saturday", 
                                                   "Sunday"),
                                1,0), 
             month=month(date, label=TRUE, abbr=TRUE),
             month.fyear=month(date, label=TRUE, abbr=TRUE) %>% 
                   paste(fyear, sep="-") %>% 
                   as.factor) %>% 
      filter(month %in% c("Oct", "Nov", "Dec", "Jan", "Feb"))

str(visits)
summary(visits)

