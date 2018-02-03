
#**********************************************
# FUNCTION TO TAKE FYEAR, P.SEG, RETURN P.SEG SOURCES DISTRIBUTION FROM LAST 
# YEAR 
#**********************************************

sources.dist <- function(dataframe, f.year, p.segment){
      # dataframe is all records 
      # fyear in format "14-15" 
      # p.seg eg. "PS13"
      
      df1 <- dataframe %>% 
            filter(fyear == f.year, 
                   p.seg.code == p.segment)
      
      # str(df1);
      # print(summary(df1))
            
      df1 <- df1 %>%
            group_by(p.seg.code, p.seg.last.year.code) %>% 
            summarise(nrow = n()) 
      
      proportions <- sapply(df1$nrow, function(x){x/sum(df1[ , 3])})
      
      df1 <- df1 %>% 
            mutate(proportions = proportions) %>% 
            as.data.frame 
      
      
      # create df with list of all p.segs to join on: 
      single.digit.nums <- paste0("0", 1:9)
      p.segs.vector <- c(paste0("PS", single.digit.nums),
                         paste0("PS", 10:14))
      
      p.seg.list <- 
            data.frame(p.seg.last.year.code = as.factor(p.segs.vector))
      
      # now join df1 with the reference list: 
      df1 <- full_join(df1, p.seg.list) %>% 
            mutate(p.seg.last.year.code = factor(p.seg.last.year.code, 
                   levels = c(p.segs.vector, NA))) %>% 
            arrange(p.seg.last.year.code)
            
      # add col with current fyear:
      df1 <- df1 %>% 
            mutate(fyear = rep(f.year, nrow(df1)))
                                    
      return(df1)
}

