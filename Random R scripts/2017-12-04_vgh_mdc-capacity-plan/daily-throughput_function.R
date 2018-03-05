

#*******************************************
# GENERATING RESULTS FOR A SINGLE DAY 
#*******************************************

# rm(list = ls())


# TODO: ---------------
# > something wrong with sampling for RBCI?? Should be around 4 hrs, not 1-1.5! 
# > drop assumption that fractional treatments are possible (currently doing that 
#     for simplicity, hoping to average out over days)


#*****************************************

# function definition: -----------------
daily.throughput <- function(durations, block.df, 
                             hours.open=10, num.bays=9, 
                             day.number = 1){
      #*********************************
      # inputs: 
            # duration list
            # block of treatments with relative proportions
            # hours of operation for this day 
            # numbays 
      # output: df with num cases for each type of treatment in the block
      #*********************************
      
      # find out how long the block will take today: --------
      block.df %<>% 
            mutate(duration = 
                        sapply(block.df$treatment, 
                               function(x){
                                     treatment.name <- x 
                                     # treatment.duration.ecdf <- durations[[treatment.name]]
                                     sample(durations[[treatment.name]], 1, 
                                            replace=TRUE)
                               }), 
                   total.time = num.cases.per.block * duration)
      
      #***********************************************
      # incredibly inelegant hack to fix sampling for RBCI: 
      block.df$duration[3] <- sample(durations[["RBCI"]], 1)  # %>% print 
      block.df %<>% 
            mutate(total.time = num.cases.per.block * duration) 
      #***********************************************
      
      # now find total time: 
      block.total.time <- sum(block.df$total.time)
      
      
      # how much capacity do we have today? ----------------
      capacity <- hours.open * num.bays
      
      
      # how many blocks will fit in the capacity? ----------
      num.blocks.today <- capacity %/% block.total.time  # todo: use integer division? 
      
      # how much time is left over? -------------
      excess.time <- capacity - (num.blocks.today * block.total.time)
      
      # update block.df to show total num treatments: --------
      block.df %<>% 
            mutate(num.cases.today = num.cases.per.block * num.blocks.today, 
                   day.number = rep(day.number, nrow(block.df)))
      
      
      # put the results together: --------------
      results <- list(block.result = block.df, 
                      block.total.time = block.total.time, 
                      num.blocks.today = num.blocks.today, 
                      excess.time = excess.time)

      return(block.df)
      
      
      
}



# test the function: -----------------

# get data; 
source("2017-12-05_vgh_mdc-treatment-duration-ecdfs.R")
source("2017-12-07_vgh_mdc-generate-block.R")

# run fn: 
daily.throughput(list1.durations, block)

# replicate(10, daily.throughput(list1.durations, block)) 

lapply(1:100, 
       daily.throughput,  # name of function 
       durations = list1.durations,  # orther args 
       block.df = block, 
       hours.open=10, 
       num.bays=9)
