

#*******************************************
# EXPLORING BOOKING DATA: FIND LOSSES DUE TO CANCELS, NO-SHOWS, etc. 
#*******************************************

library("tidyr")
library("ggplot2")
library("magrittr")
library("dplyr")
library("reshape2")
library("lubridate")

# rm(list = ls())

source("lost.hrs_function.R")

#*************************************
# read in booking data: 
data.path <- "H:/VCH files - Nayef/2017-12-04_vgh_mdc-capacity-plan/results/clean data"

df1.booking.data <- read.csv(file= paste0(data.path, "/2017-12-13_vgh_mdc-booking-data.csv"))

# data wrangling: 
names(df1.booking.data) <- tolower(names(df1.booking.data))
df1.booking.data %<>% 
      mutate(status = as.character(status))

# replace "IN" with "xIN" to arrange alphabetically: 
df1.booking.data$status[df1.booking.data$status=="IN"] <- "xIN"

df1.booking.data %<>%       
      mutate(id = as.factor(id), 
             appt..date = dmy(appt..date), 
             txcode = as.factor(txcode), 
             status = as.factor(status), 
             starttime = as.POSIXct(strptime(as.character(starttime), 
                                  format="%R")), 
             endtime = as.POSIXct(strptime(as.character(endtime), 
                                           format="%R"))) %>% 
      rename(date = appt..date, 
             bay = chair.bed)


str(df1.booking.data)
summary(df1.booking.data)


# split the data by date and chair
list1.split <- split(df1.booking.data,
                     list(df1.booking.data$date,
                                         df1.booking.data$bay))

# remove dfs with 0 rows 
list1.split <- list1.split[sapply(list1.split, function(x) dim(x)[1]) > 0]

str(list1.split)


# find losses due to non-IN patients: ----------------
lost.hrs(list1.split[[1999]])

list2.losses <- lapply(list1.split, lost.hrs)
df2.losses <- do.call(rbind, list2.losses)





# write result: 
output.path <- "H:/VCH files - Nayef/2017-12-04_vgh_mdc-capacity-plan/results/output from src"

write.csv(df2.losses, 
          file  = paste0(output.path, "/2017-12-21_vgh_mdc-losses-due-to-cancels.csv"), 
          row.names = FALSE)
