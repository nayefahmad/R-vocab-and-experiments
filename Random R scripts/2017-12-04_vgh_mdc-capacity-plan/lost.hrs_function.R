

#*******************************************
# FUNCTION TO FIND LOST HOURS IN BOOKING DATA: 
#*******************************************

# rm(list = ls())

# source("2017-12-13_vgh_mdc-explore-booking-data.R")

# TODO: ---------------



# fn defn: 
lost.hrs <- function(df) {
      # dataframe is booking data split on date and bay 
      # output: dataframe with added col showing lost hours
      #     note: this doesn't count gaps where two appointments are not 
      #     scheduled back-to-bacck. Only looks at gaps caused by no-shows, 
      #     cancelled appointments, etc. 
      
      nrow = nrow(df)  # %>% print 
      
      if (nrow > 1) {
            lag.end <- df$endtime[1:(nrow-1)]   # %>% print
            lag.start <- df$starttime[1:(nrow-1)]  # %>% print 
      } else {
            
            # accounting for dfs with only 1 row: 
            if (df$status[1] == "xIN") {
                  result.df <- data.frame(date = df$date, 
                                          bay = df$bay, 
                                          loss.time = df$starttime,
                                          losses = 0, 
                                          losses.made.up = 0)
                  return(result.df) 
            } else {
                  result.df <- data.frame(date = df$date, 
                                          bay = df$bay,
                                          loss.time = df$starttime,
                                          losses = df$endtime[1] - df$starttime[1], 
                                          losses.made.up = 0)
                  return(result.df) 
                         
            }
      }
      
      
      
      # first we ensure that df is arranged right: 
      df %<>% arrange(starttime, status) # %>% print 
      
      # check first row for losses: 
      first.row.loss <- ifelse(!(df$status[1] %in% "xIN"), 
                               df$endtime[1] - df$starttime[1], 
                               0) # %>% print 
      
      # save starttime data before removing first row: 
      loss.time <- df$starttime 
      
      # now we have to remove first row to add lagged vars: 
      df <- df[2:nrow, ]
      # df$starttime %>% print 
      # print(df$starttime - lag.end) 
      
      df %<>% 
            select(id, date, bay, status, starttime, endtime) %>% 
            mutate(# add lag vars:
                  lag.starttime = lag.start,  
                  lag.endtime = lag.end,
                  
                  # durations: 
                  dur = endtime - starttime, 
                  lag.dur = lag.endtime - lag.starttime, 
                   
                   # anything other than "xIN" is "lost" time: 
                   loss.hrs = ifelse(!(status %in% "xIN"),
                                endtime - starttime, 
                                NA), 
                   
                   is.overlap = ifelse(status %in% "xIN", 
                                       ifelse(starttime < lag.endtime, 
                                              1, 
                                              0), 
                                       NA), 
                   loss.makeup = ifelse(is.overlap == 1,
                                       ifelse(dur >= lag.dur, 
                                              
                                              # branch 1: 
                                              ifelse(starttime == lag.starttime, 
                                                     lag.dur, 
                                                     (lag.dur - (starttime - lag.starttime))), 
                                              
                                              # branch 2: 
                                              ifelse(starttime == lag.starttime, 
                                                     lag.dur, 
                                                     (lag.dur - (starttime - lag.starttime)))),
                                       NA)) 
                                              
                                              
      # print(df)                                
      
      # aggregate all losses: 
      losses <- c(first.row.loss, df$loss.hrs)
      losses[is.na(losses)] <- 0
      
      losses.made.up <- c(df$loss.makeup, NA) 
      losses.made.up[is.na(losses.made.up)] <- 0 
      
      result.df = data.frame(date = rep(df$date[1], length(losses)), 
                             bay = rep(df$bay[1], length(losses)),
                             loss.time = loss.time, 
                             losses = losses, 
                             losses.made.up = losses.made.up)
      
      return(result.df)
      
     
}


# test the fn: 
# lost.hrs(list1.split[[2]])
