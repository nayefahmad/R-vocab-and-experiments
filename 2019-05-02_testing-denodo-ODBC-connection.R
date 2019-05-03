
#*******************************************************************
# Testing ODBC connection to denodo 
# 2019-05-02
# Nayef 

#*******************************************************************

library(odbc)
library(dbplyr)
library(magrittr)  # not really essential 
library(tidyverse)


# References: ---------
# > also see file "2018-10-09_dbplyr-package-test-drive.R"
# > https://community.denodo.com/tutorials/browse/basics/4connect2odbcclient


# 1) set up database connection: -----------
# Note that we need a 64-bit ODBC connection, so in Windows start menu, use
# "ODBC Data Sources (64-bit)"

cnx <- dbConnect(odbc::odbc(),
                 dsn = "cnx_denodo_spappcsta001")





# 2) connect to view: ----------
vw_eddata <- dplyr::tbl(cnx, 
                     dbplyr::in_schema("publish", 
                                       "emergency"))

# str(vw_eddata)

vw_adtc <- dplyr::tbl(cnx, 
                   dbplyr::in_schema("publish", 
                                     "admission_discharge"))
# str(vw_adtc)





# 3) example ED query: ----------
# show the sql syntax: 
vw_eddata %>% 
    filter(facility_name == 'Lions Gate Hospital', 
           start_date_id == '20190501') %>% 
    select(patient_id, 
           is_admitted) %>%
    show_query()


# collect results from the database: 
df1.visits <- 
    vw_eddata %>% 
    filter(facility_name == 'Lions Gate Hospital', 
           start_date_id == '20190501') %>% 
    select(patient_id, 
           is_admitted) %>%
    collect()

# df1.visits 




# 4) example adtc query: -----
df2.admits <- 
    vw_adtc %>% 
    filter(facility_name == 'Lions Gate Hospital', 
           admission_date_id == '20190501') %>% 
    select(patient_id, 
           nursing_unit_at_admit) %>% 
    collect()

# df2.admits



