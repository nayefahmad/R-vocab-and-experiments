

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

# 1) set up database connection: -----------
# > using RODBC package (not recommended) ----
# cnx <- odbcConnect("cnx_SPDBSCSTA001")  

# > using odbc package (preferred) ----- 
cnx2 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SPDBSCSTA001")
# note the tab "Connections" in top right in RStudio. 


# connect to EDMart.dbo.vwEDVisitIdentifiedRegional: 
eddata <- dplyr::tbl(cnx2, 
                     dbplyr::in_schema("EDMart.dbo", 
                                       "vwEDVisitIdentifiedRegional"))

# str(eddata)
# eddata is a database object? 


# 2) working with ED data: ----------

# age distribution of patients in selected facility: 
site.param <- "UBCH"
date.param <- "2018-10-07"

q1.vgh.visits <- eddata %>%
      dplyr::filter(FacilityShortName == site.param, 
                    StartDate > date.param) %>% 
      dplyr::select(FacilityShortName, 
                    Age)

q1.vgh.visits

# histogram: 
q1.vgh.visits %>% pull(Age) %>% 
      hist(breaks = 10, 
           xlim = c(0,100), 
           main = paste0(site.param, " - Age distribution"))


# > let's write a fn for that: 
# fn definition: 
hist.age <- function(site,
                     dbobject=eddata,
                     min.date = "2018-01-01"){
      # dbobject is a list "$ con  :Formal class 'Microsoft SQL Server'"
      # e.g. site = "VGH", 
      # date = "2018-01-01"
      
      # pull site and age fields: 
      q1.vgh.visits <- dbobject %>%
            dplyr::filter(FacilityShortName == site, 
                          StartDate > min.date) %>% 
            dplyr::select(FacilityShortName, 
                          Age)
      
      # histogram: 
      q1.vgh.visits %>% 
            pull(Age) %>% 
            hist(breaks = 10, 
                 xlim = c(0,100), 
                 main = paste0(site, " - Age distribution of ED Visits"))
      
}

# fn test: 
sites <- c("VGH", "SPH", "RHS", "UBCH")
lapply(sites, 
       hist.age)


# collect data from the database as a df: 
df1.vgh.visits %>% dplyr::collect()








#************************************************
# 3) adtc data: ----------
#************************************************
q2.adtcdata <- dplyr::tbl(cnx2, 
                          dbplyr::in_schema("ADTCMart.ADTC", 
                                            "CensusView"))

# top nursing unit in terms of num census patients : 
df2.unit.data <- q2.adtcdata %>% 
      filter(CensusDate == "2018-10-01", 
             FacilityLongName == "Vancouver General Hospital") %>% 
      select(NursingUnitCode) %>% 
      dplyr::collect() %>%  # download the data 
      group_by(NursingUnitCode) %>% 
      summarize(num.pts = n()) %>% 
      arrange(desc(num.pts)) %>% 
      mutate(NursingUnitCode = as.factor(NursingUnitCode))
      
str(df2.unit.data)

# plot: 
df2.unit.data %>% 
      ggplot(aes(x = reorder(NursingUnitCode, num.pts), 
                 y = num.pts)) + 
      geom_bar(stat = "identity") + 
      coord_flip()
