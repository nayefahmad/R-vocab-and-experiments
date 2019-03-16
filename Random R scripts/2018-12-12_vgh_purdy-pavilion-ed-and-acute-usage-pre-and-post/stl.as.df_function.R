

#************************************************
# FUNCTION TO RUN STL DECOMPOSITION AND SAVE RESULTS AS DF
#************************************************

# define function: 
stl.as.df_fn <- function(x){
      # This is an example of a custom function
    
      # input/argument: x, a time series object (Class = ts)
      # output: a dataframe 
      
      stl <- stl(x, s.window = "periodic")
      
      stl.df <- stl[[1]] %>% as.data.frame() %>% 
            mutate(data = seasonal + trend + remainder, 
                   timeperiod = seq_along(seasonal))
      
      return(stl.df)
}

# test function: 
# stl.as.df_fn(df1.deaths.data$deaths.ts[[5]])
