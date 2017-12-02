

#******************************************
# function to find LOS of single patient
#******************************************

# rm(list=ls())

# todo: --------

#---------------


# example data ----------
# source("2017-10-06_Example-data-cleaning.R")
# str(losdata)

# split dataframe to create 4 separate dfs : 
# split.losdata <- split(losdata, losdata$id)
# str(split.losdata)


# function definition: ----------------

los.fn <- function(df, nursingunit){
      # inputs:
            # dataframe with all rows of single patient-account combo 
            # nursingunitcode as character string 
      # output: LOS in 4E in days 
      
      require("dplyr")
      require("tidyr")
      require("lubridate")
      
      
      df <- arrange(df,
                    ad.dtime, t.dtime)
      # print(df)
      # df$ad.unitcode
      if (df$ad.unitcode[1] == nursingunit && is.na(df$t.dtime[1] == TRUE)) {
            # patient type: ad and dis from 4E, no transfers 
            # print("branch1")
            difftime <- df$dis.dtime - df$ad.dtime
            return(as.numeric(difftime, units="days"))
            
      } else if (df$ad.unitcode[1] == nursingunit && df$to.unit != nursingunit){
            # patient type: admit to 4E, transferred out of 4E, 
            # no internal transfers in 4E 
            # print("branch2")
            difftime <- df$t.dtime[1] - df$ad.dtime[1]
            return(as.numeric(difftime, units="days"))
            
      } else if (df$ad.unitcode[1] == nursingunit && df$to.unit == nursingunit){
            # patient type: admit to 4E, internal transfer in 4E
            index <- df$to.unit != nursingunit  
            # ^ logical vec to find first transfer out of 4E 
            # print("branch3")
            # print(c("index=", index))
            
            # check whether transfer or discharge is the endpoint of LOS: 
            if (any(index)==TRUE){
                  i <- match(TRUE, index)  # rownum of transfer out of 4E 
                  difftime <- df$t.dtime[i] - df$ad.dtime[1]
                  return(as.numeric(difftime, units="days"))
            } else {
                  difftime <- df$dis.dtime[1] - df$ad.dtime[1]
                  return(as.numeric(difftime, units="days"))
            }
            
      } else if (df$ad.unitcode[1] != nursingunit && df$to.unit == nursingunit) {
            # patient type: admit to other, transferred to 4E 
            index <- df$to.unit != nursingunit  
            # print("branch4") 
            # print(c("index=", index))
            
            # check whether transfer or discharge is the endpoint of LOS: 
            # note that t.dtime[1] is the start point, not ad.dtime[1]
            if (any(index)==TRUE){
                  i <- match(TRUE, index)  # rownum of transfer out of 4E 
                  difftime <- df$t.dtime[i] - df$t.dtime[1]
                  return(as.numeric(difftime, units="days"))
            } else {
                  difftime <- df$dis.dtime[1] - df$t.dtime[1]
                  return(as.numeric(difftime, units="days"))
            }
            
            
      } else {
            # patient type: other 
            # print("branch5")
            return(NA)
      }
}



# test the function over each df in split.losdata: ------------
# lapply(split.losdata, los.fn)  %>% unlist # %>% unname %>% str
# 
# running separately on each element: 
# los.fn(split.losdata[[1]])
# los.fn(split.losdata[[2]])
# los.fn(split.losdata[[3]])
# los.fn(split.losdata[[4]])
