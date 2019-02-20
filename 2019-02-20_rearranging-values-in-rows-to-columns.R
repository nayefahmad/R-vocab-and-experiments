
#**********************************************************
# EXAMPLE: REARRANGING VALUES IN ROWS TO SEPERATE COLUMNS
# 2019-02-20
# Nayef 

#**********************************************************


library(tidyverse)
library(tidyr)


# set up example data: -------------
df1.data <- 
    data.frame(stringsAsFactors=FALSE,
               patientid = c(1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
               team = c("a", "b", "c", "d", "c", "t", "a", "e"))  # %>% 
    
    # mutate(id = 1:n()) %>% 
    # select(id, 
    #        everything())

# result: 
# df1.data


# group by and nest: ---------
df2.nested <- 
    df1.data %>% 
    group_by(patientid) %>% 
    nest

df2.nested
df2.nested$data

# modify nested dataframe using spread: ----------
df3.spread <- 
    df2.nested %>% 
    mutate(spread_teams = map(data, 
                              
                              # this function just transposes the team names, and creates 
                              #     10 columns for each patient
                              function(x){
                                  c(t(x$team), rep("NA", 10-length(x$team)))  # todo: 10 is assumed to be the max number of rows
                                  }))

# df3.spread


# return dataframe with 1 row for each patient, and 
bind_cols(df3.spread[1], 
          as.data.frame(do.call(rbind, df3.spread$spread_teams)))
    




