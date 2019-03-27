

#******************************************************
# Nonstandard evaluation example 
# 2019-03-27
# Nayef 

# Reference: https://www.linkedin.com/feed/update/urn:li:activity:6506312394439041024/
#******************************************************

# Let's assume you want a data summarizing function, which returns a set of
# descriptive statistics. You would like to drive the scope of returned measures
# with some user interface (checkboxes) or just "switches" (TRUE/FALSE).
# Sometimes you need just "mean + sd", other time it's "N and %" or "median +
# qd". You would also like to pass names of other functions (including your own)
# ad hoc, to make it flexible.

# So, what you're going to do, is to hardcode a basic (yet exhaustive) set of
# functions you usually use, then make it selectable.

# You may ask: "well, couldn't I just compute it all, then filter the output by
# names?". Yes, but it runs all the functions. In my solution - it saves
# resources.


# function defn: ----

my_summary <- function(data, var, switches, ...){
    
    if(!require(dplyr)){library(dplyr)}
    
    return(data %>% 
               summarize_at(vars(!!enquo(var)), 
                            .funs = funs(N = n(), 
                            ...) [switches])
    )
               
    
}



# test the function: -----
df <- data.frame(x = rnorm(100), 
                 y = rpois(100, 10))


my_summary(df, x)



