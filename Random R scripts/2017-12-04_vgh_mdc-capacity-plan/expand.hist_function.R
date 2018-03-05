

#**************************
# FN TO EXPAND HISTOGRAM TO FULL DATA     
#**************************

expand.hist <- function(df, data.col){
      # dataframe: df1
      # data.col: col number to expand; cannot be 1 
      # output: vector with expanded data 
      
      ref.data <- df[ ,1]
      
      data.vec <- df[ ,data.col]
      
      return(mapply(rep, ref.data, data.vec) %>% 
                   unlist)  
      
      
}


# test the fn: ----------
# fake data: 
# df1 <- data.frame(a=10:20, 
#                  b=1:11) %>% print 
# 
# expand.hist(df1, 2)
# expand.hist(df1, 2) %>% table
