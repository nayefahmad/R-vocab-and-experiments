
#'--- 
#' title: "pins package test drive"
#' author: "Nayef Ahmad"
#' date: "2020-01-30"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: hide
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     toc_folding: false
#' ---

#' Reference: https://github.com/rstudio/pins 
#' 

library(tidyverse)
library(pins)

#' ## Option 1:
#' 
#' Pin a remote file with `pin(url)`. This will download the file and make it
#' available in a local cache. This makes subsequent uses much faster and allows
#' you to work offline. If the resource changes, pin() will automatically
#' re-download it; if goes away, pin() will keep the local cache.
#' 

url <- "https://raw.githubusercontent.com/facebook/prophet/master/examples/example_retail_sales.csv"
retail_sales <- read.csv(pin(url))


#' ## Option 2: 
#' 
#' Pin an expensive local computation with `pin(object, name)`
#' 

retail_sales %>%
    group_by(month = lubridate::month(ds, T)) %>%
    summarise(total = sum(y)) %>%
    pin("sales_by_month")


#' Then later retrieve it with `pin_get(name)`. 
#' 

rm(list = ls())
pin_get("sales_by_month")












