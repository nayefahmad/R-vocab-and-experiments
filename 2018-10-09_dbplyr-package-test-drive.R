

#*********************************************
# dbplyr package test drive 
# 2018-10-09
# Nayef 

#*********************************************

library(odbc)
library(dbplyr)
library(magrittr)  # not really essential 
library(tidyverse)

# References: 
# > https://github.com/r-dbi/odbc#connection-strings
# > https://towardsdatascience.com/how-to-write-tidy-sql-queries-in-r-d6d6b2a3e17 

# 1) set up database connection: -----------

# > 1.1) using RODBC package (not recommended) ----
# cnx <- odbcConnect("cnx_SPDBSCSTA001")  


# > 1.2) using odbc package (preferred) ----- 
cnx2 <- dbConnect(odbc::odbc(),
                  dsn = "cnx_SPDBSCSTA001")

# note the tab "Connections" in top right in RStudio. 


# > 1.3) connect to EDMart.dbo.vwEDVisitIdentifiedRegional: -----
eddata <- dplyr::tbl(cnx2, 
                     dbplyr::in_schema("EDMart.dbo", 
                                       "vwEDVisitIdentifiedRegional"))

# str(eddata)


# 2) Example: working with ED data: ----------

# age distribution of patients in selected facility: 
site.param <- "UBCH"
date.param <- "2018-10-07"

# write query in dplyr syntax: 
q1.vgh.visits <- 
    eddata %>%
    filter(FacilityShortName == site.param, 
           StartDate > date.param) %>% 
    select(FacilityShortName, 
           Age)

# str(q1.vgh.visits)

q1.vgh.visits %>% show_query()  # convert dplyr syntax to sql syntax 



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
