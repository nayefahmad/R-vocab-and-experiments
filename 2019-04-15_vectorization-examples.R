

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
# > 


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



# > 1.2 vectorized solution: 

