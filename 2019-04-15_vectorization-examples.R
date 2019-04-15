

#*******************************************************************
# Vectorization examples 
# 2019-04-15
# Nayef 

#*******************************************************************


#*******************************************************************
# Example 1 -------------

# Reference: https://www.r-bloggers.com/how-to-use-vectorization-to-streamline-simulations/

# Write a program to: 
# > toss a coin n times and print out after every 100 tosses the proportion of heads minus 1/2
# > print out after every 100 tosses the number of heads minus half the number of tosses
# > do these numbers appear to approach 0 as n increases? 


#*******************************************************************



# > 1.1) non-vectorized solution: ----------

# function definition: 
coin_toss <- function(n){
    
    result <- c()  # empty vector 
    
    for (i in 1:n) {
        
        if (i == 1){
            # the first flip we just assign the toss result to variable "tosses"
            tosses <- sample(c(0,1), 1)
            
        } else {
            # creating a vector, "tosses" that has history of all tosses
            tosses <- c(tosses ,sample(c(0,1), 1))
            
        }
        
        # when we reach a toss number that a multiple of 100 we output the status
        if (i %% 100 == 0) {
            ## output the percent of heads away from 50%
            percent <- (sum(tosses) / length(tosses)) - 0.5
            
            ## output the number of heads away from half of all tosses
            number <- sum(tosses) - (length(tosses) / 2)
            
            # join together in var "result" 
            result <- rbind(result, c(percent, number))
        }
    }
    
    return(result)
}


# function test: 
set.seed(10)
coin_toss(1000)



# > 1.2 vectorized solution: ----------

# function definition: 
coin_toss_vectorized <- function(n, step = 100) {
    
    if (n < 100) {return(cat("n must be greater than 100"))}
    
    # Record num heads at each step
    tosses <- cumsum(sample(c(0, 1), n, replace = TRUE))
    
    # define step for summaries: 
    steps <- seq(step, n, by = step)
    
    # Compute summaries
    percent <- tosses[steps] / steps - .5
    number <- tosses[steps] - steps/2
    
    # retuen result
    return(cbind(percent, number))
    
}


# function test: 
set.seed(10)
coin_toss_vectorized(1000)

set.seed(10)
coin_toss(1000)

# vectorized and non-vectorized functions give same result 


# compare speeds: 
system.time(coin_toss(100000))  # elapsed = 12.65 sec
system.time(coin_toss_vectorized(100000))  # elapsed = 0 sec!!!
