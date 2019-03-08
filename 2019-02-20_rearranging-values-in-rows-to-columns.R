
#**********************************************************
# EXAMPLE: REARRANGING VALUES IN ROWS TO SEPERATE COLUMNS
# 2019-02-20
# Nayef 

# Problem: data has several rows per patient, with each row corresponding to one
# Community team that they have an active referral with. We want to rearrange
# the data so that there's one row per patient, with each team in a separate
# column (long format to wide format). Patients can be associated with different
# number of teams

#**********************************************************


library(tidyverse)


# set up example data: -------------
df1.data <- 
    data.frame(stringsAsFactors=FALSE,
               patientid = c(1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L),
               team = c("a", "b", "c", "d", "c", "t", "a", "e"))  

# result: 
# df1.data


# df1.data <- read_csv("G:/QUIST/Production/Deepak K/2019-02-20_team-names.csv") %>% 
#     rename(patientid = PatientID, 
#            team = `Team Moved`)


# group by and nest: ---------
df2.nested <- 
    df1.data %>% 
    group_by(patientid) %>% 
    nest

df2.nested
# df2.nested$data

# modify nested dataframe: ----------
df3.spread <- 
    df2.nested %>% 
    mutate(spread_teams = map(data, 
                              
                              # this function just transposes the team names, and creates 
                              #     10 columns for each patient
                              function(x){
                                  c(t(x$team),  # transpose the column of team names 
                                    rep(NA, 10-length(x$team)))  # todo: 10 is assumed to be the max number of rows; is this right? 
                                  }))

# result: 
# df3.spread


# return dataframe with 1 row for each patient, and 
df4.output <- bind_cols(df3.spread[1], 
                        as.data.frame(do.call(rbind, df3.spread$spread_teams)))

# write_csv(df4.output, 
#           "G:/QUIST/Production/Deepak K/2019-02-20_team-names-rearranged.csv")    




