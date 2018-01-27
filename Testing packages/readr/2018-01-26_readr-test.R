
#***************************
# READR PACKAGE 
#***************************

library("readr")
library("here")
library("magrittr")

# Apparently readr is faster and more reproducible than base R 

# read a csv: ----
df1 <- read_csv(paste0(here("data"), 
                "/Lunch&Learn data.csv"))

# inline csv: can be a quick way to make a tbl: ----
read_csv("a, b, c
         1, 2, 3
         10, 14, 19")

# you can copy in from notepad: 
read_csv("a, g
         1, 456
         3, 665
         3, 556
         9, 1000")



#***************************************
# parse_*( ) functions ----
#***************************************
# Like all functions in the tidyverse, the parse_*() functions are uniform: the first argument is a character vector to parse, and the na argument specifies which strings should be treated as missing

# for details see 
# http://r4ds.had.co.nz/data-import.html

parse_logical(c("TRUE", "FALSE", "NA")) %>% str
#>  logi [1:3] TRUE FALSE NA

parse_integer(c("1", "2", "3", "."), 
              na = ".") %>%  # specify NA value code 
      str
#>  int [1:3] 1 2 3

parse_date(c("2010-01-01", "1979-10-14")) %>% str
#>  Date[1:2], format: "2010-01-01" "1979-10-14"



# parse numbers ------
parse_number(c("$100",
               "$20", 
               "4.4%", 
               "I paid $43.2"))

# parse factors: ------
# need to specify levels
cols = c("red", "green")
parse_factor(c("red", "red", "green"), 
             levels = cols)

parse_factor(c("red", "red", "green", "blue"), 
             levels = cols[1])  # error 


# parse dates: -----
# 3 options: parse_date, parse_datetime, parse_time

parse_time(c("14:52", 
             "2:00 am", 
             "6:02 pm", 
             "2018-01-24 09:00 am"))


#********************************
# write csvs: 