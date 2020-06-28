
#*******************************************************
# Source several files in a loop  
# 2020-06-27
# Nayef 
#*******************************************************

library(here)

files <- 
    here("Random R scripts",
         "random_functions") %>% 
    list.files(full.names = TRUE)

sapply(files, source)

# test functions 
fn1()
fn2()
