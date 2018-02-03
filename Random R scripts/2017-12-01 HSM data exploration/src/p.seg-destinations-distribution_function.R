

#**********************************************
# FUNCTION TO TAKE FYEAR-END, P.SEG, RETURN P.SEG DESTINATION DISTRIBUTION 
# AFTER x YEARS 
#**********************************************


# function definition: ---------
destinations.dist <- function(destination.col, p.seg, dataframe){
      # df is data for all hsm.ids, p.segs in 09-10, left-joined with p.segs 
      #     for 10-11, 11-12, etc. 
      # destination.col specifies the transition duration of interest by 
      #     giving the col number for the year 
      # p.seg: p.seg of interest in 09-10 data 
      
      # output: df with all destinations from p.seg, with volumes and 
      #     proportions 
      
      
      df1 <- dataframe %>% 
            select(1, 2, destination.col) %>%  # hsm.id, 09-10 psegs, and destination 
                                               # year p.segs. 
            filter(p.seg.code.09_10 == p.seg)
      
      
      # set colnames to use in group_by_:  (note that group_by() doesn't work here)
      dest.year.colname <- names(df1[3])  # %>% print 
      p.seg.code.09_10 <- "p.seg.code.09_10"
         
      
      df1 <- df1 %>%
            group_by_(p.seg.code.09_10, dest.year.colname) %>%
            summarise(nrow = n())

      proportions <- sapply(df1$nrow, function(x){x/sum(df1$nrow)})  # %>% print 
      
      df1 <- df1 %>%
            mutate(proportions = proportions) %>%
            as.data.frame
      
      # create df with list of all p.segs to join on: 
      single.digit.nums <- paste0("0", 1:9)
      p.segs.vector <- c(paste0("PS", single.digit.nums),
                         paste0("PS", 10:14))
      
      p.seg.list <- 
            data.frame(p.seg.list = as.factor(p.segs.vector))
      
      # now join df1 with the reference list: 
      names(df1)[2] <- "p.seg.list"  # necessary for joining  
      df1 <- full_join(df1,
                       p.seg.list)
      names(df1)[2] <- dest.year.colname
      
      # change psegs to factor, arrange by dest psegs (dplyr not working well)
      df1[ , 2] <- factor(df1[ , 2], 
                          levels = c(p.segs.vector, NA))
      
      df1 %<>% arrange(df1[ , 2])
      
      # replace NAs in first col with first non-NA value: 
      df1$p.seg.code.09_10[is.na(df1$p.seg.code.09_10)] <- 
            df1$p.seg.code.09_10[match(FALSE, is.na(df1$p.seg.code.09_10))] 
      
      # replace NAs in other cols with 0s: 
      df1$nrow[is.na(df1$nrow)] <- 0 
      df1$proportions[is.na(df1$proportions)] <- 0 
      
      return(df1)

      
}


# test the fn: ---------
# destinations.dist(3, "PS01", join.t1.1_t1.2)
