
#****************************************************************
# Lag() and Lead() to bring sequential rows into columns 
# 2019-05-02
# Nayef 
#****************************************************************

library(tidyverse)
library(magrittr)

options(readr.default_locale=readr::locale(tz="America/Los_Angeles"))

# rm(list = ls())

# todo: distribution of where people go after ACWR
# todo: add in timestamps 


# 1) Problem statement: -------------

# You have data on the locations where patients are in a hospital, with
# timestamps for each location. You want to modify the data so that at every
# row, there is a new column that gives "next location".

# Data: --------------
# As an example: 
df1.location_data <- tibble::tribble(
        ~patient_ID, ~location,
                  1,    "ACWR",
                  1,    "ACWR",
                  1,  "INTKWR",
                  1,    "INTK",
                  1,    "INTK",
                  1,    "INTK",
                  2,    "ACWR",
                  2,    "ACWR",
                  2,      "AC",
                  2,      "AC"
        )

# Required result:------------ 
tibble::tribble(
    ~patient_ID, ~location, ~next_location,
              1,    "ACWR",         "ACWR",
              1,    "ACWR",       "INTKWR",
              1,  "INTKWR",         "INTK",
              1,    "INTK",         "INTK",
              1,    "INTK",         "INTK",
              1,    "INTK",             NA,
              2,    "ACWR",         "ACWR",
              2,    "ACWR",           "AC",
              2,      "AC",           "AC",
              2,      "AC",             NA
    )



# Solution using dplyr::lead(): ----------
?lead()

df1.location_data %<>% 
    group_by(patient_ID) %>% 
    mutate(next_location = lead(location)) %>% print
    

# you can also add columns for location_2steps_forward, location_3steps_forward,
# etc

df1.location_data %<>% 
    mutate(location_2steps_forward = lead(location, 2), 
           location_3steps_forward = lead(location, 3)) %>% print    




# 2) Problem statement 2: -----------

# for a given location, what's the distribution of frequencies of going to each
# of the other locations? 

# Required result: ---------
# Distribution of "destinations" from ACWR as a starting point. 

tibble::tribble(
    ~start_loc, ~end_loc, ~count,
        "ACWR", "INTKWR",      1,
        "ACWR",     "AC",      1
    )

# Note that from the data above, we want to exclude rows where both the location 
#   and next_location are ACWR 







