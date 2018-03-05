

#******************************************* 
# FN TO FIND CAPACITY REQUIRED: 
#*******************************************

# rm(list = ls())


# TODO: ---------------
# > include numbays, hrs. per day, etc in final results 

#***************************************
# fn definition: ------
capacity.required.full.fy <- function(rep.number=1,
                              services.list, 
                              durations.data, 
                              num.bays = 9,
                              hr.per.day = 10,
                              days.open = 250, 
                              full.results = 1){
      
      # inputs: 
            # df with services list and volumes; 
            # duration data in expanded histogram format (a list)
            # full.results:         1== FALSE, only show total capacity 
                              #     2== TRUE, also show breakdown 
                              #     3== show breakdown only 
      # output: hours required to meed this volume; hours available given 
            # input num.bays and days.open, and difference

      
            
      #*************************************
      # create fn to take 2 cols from durations.data, 
      #      returns a random sample from observed distribution: 
      generate.durations <- function(col1.treatment, col2.volume){
            col1.treatment <- as.character(col1.treatment)
            full.data <- sample(durations.data[[col1.treatment]],
                                col2.volume,
                                replace = TRUE)
            # print(durations.data[[col1.treatment]])
            # print(sum(full.data))
            return(sum(full.data))
      }
      
      
      for (i in 1:nrow(services.list)){
            generate.durations(services.list$code[i], services.list$volume[i])
      }
      
      
      services.list %<>%
            mutate(total.time = mapply(generate.durations,
                                       services.list$code,
                                       services.list$volume),

                   # put rep number in results:
                   replication = rep(rep.number, nrow(services.list)))
      # print(services.list)

      # total demand: ------
      demand <- sum(services.list$total.time)

      # total supply: ------
      supply <- num.bays * hr.per.day * days.open

      # results: --------
      results.list <- list(capacity = data.frame(replication = rep.number,
                                          demand = demand,
                                          supply = supply,
                                          excess.dd = demand - supply),
                           services.full = services.list)

      # choose format to display results in:
      if (full.results == 2) {
            return(results.list)
      } else if (full.results == 1) {
            return(results.list[[1]])
      } else if (full.results == 3) {
            return(results.list[[2]])
      }
      

}
