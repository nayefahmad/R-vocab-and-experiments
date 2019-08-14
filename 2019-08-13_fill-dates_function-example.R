#'--- 
#' title: "A function for filling in missing dates"
#' author: "Nayef Ahmad"
#' date: "2019-08-13"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float: true
#' ---

#+ lib, include = FALSE 
library(denodoExtractor)
library(tidyverse)
library(kableExtra)
library(DT)


#' I run into this problem often enough that I decided to create a function for
#' it and to include it in the `denodoExtractor` package. That way, the function
#' is available whenever you load the package - you don't have to keep copying
#' it over from one folder/script to another. 


#+ r 
#' ## Example data 
#' 
#' Let's say we have some admits data from a small clinic, for January 2019. 
#' 

df <- data.frame(admit_date = c("2019-01-01", 
                                "2019-01-04", 
                                "2019-01-10", 
                                "2019-01-23"), 
                 admits = c(25, 40, 12, 50))


df %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped","condensed","responsive"), 
                full_width = F)

#' \  
#' \  
#' \  
#' 
    
#' ## Problem statement
#'
#' How do I get a dataframe with 1 row for each day in January, regardless of
#' whether or not there was an admit on that date? This is not hard to do, but
#' it is a little tedious, and I want to remove as much friction as possible, so
#' that it's easy to focus on the big-picture goals of whatever analysis I'm
#' doing.
#'
#' ### Why?
#'
#' Most obiously, to plot a time series, calculate a rate with the correct
#' denominator, etc.
#' 

#' \  
#' \  
#' \  
#' 

#' ## Solution 
#' 
#' ### Install/Load the package first: 

#+ lib-install
# install the package first: 
# devtools::install_github("nayefahmad/denodoExtractor")
# library(denodoExtractor)

#' ### Check function documentation:
# ?fill_dates 


#' ### Run the function:
#' 
#' You only need to supply three arguments: 
#' 
#' * the name of the column that has dates in it 
#' * the start date of the range that you want to "fill in"
#' * the end date of the range that you want to "fill in"

#+ run-function
df %>% fill_dates(admit_date, 
                  "2019-01-01", 
                  "2019-01-31") %>% 
  
  # this part is not part of the example: 
  datatable()

#' \  
#' \  
#' \  
#' 

#' ### Quick plot
#' 

df %>% fill_dates(admit_date, 
                  "2019-01-01", 
                  "2019-01-31") %>%
  replace_na(replace = list(admits = 0)) %>% 
  
  ggplot(aes(x = dates_fill, 
             y = admits)) + 
  geom_point() + 
  geom_line()
  
  
  
