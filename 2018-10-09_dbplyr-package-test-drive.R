

#*********************************************
# dbplyr package test drive 
#*********************************************

# library(RODBC)
library(odbc)
library(DBI)
library(dbplyr)
library(magrittr)
library(tidyverse)

# References: 
# > https://github.com/r-dbi/odbc#connection-strings
# > https://towardsdatascience.com/how-to-write-tidy-sql-queries-in-r-d6d6b2a3e17 

# set up database connection: -----------
# > using RODBC package ----
# cnx <- odbcConnect("cnx_SPDBSCSTA001")  

# > using odbc package (preferred) ----- 
cnx2 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SPDBSCSTA001")
# note the tab "Connections" in top right in RStudio. 


# connect to EDMart.dbo.vwEDVisitIdentifiedRegional: 
eddata <- dplyr::tbl(cnx2, 
                     dbplyr::in_schema("EDMart.dbo", 
                                       "vwEDVisitIdentifiedRegional"))

str(eddata)
# eddata is a database object? 


# working with database data: ----------

# age distribution of patients in 
df1.vgh.visits <- eddata %>%
      dplyr::filter(FacilityShortName == "VGH", 
                    StartDate > "2018-10-07") %>% 
      dplyr::select(FacilityShortName, 
                    Age)

df1.vgh.visits

df1.vgh.visits %>% pull(Age)

