
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
                 dsn = "denodo_test")









