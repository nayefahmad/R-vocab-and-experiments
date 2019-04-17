

#****************************************************************************
# Vectorization examples 
# 2019-04-15
# Nayef 

#****************************************************************************

library(tidyverse)


# Example 1: Coin tosses -------------

# Reference: https://www.r-bloggers.com/how-to-use-vectorization-to-streamline-simulations/

# Write a program to: 
# > toss a coin n times and print out after every 100 tosses the proportion of heads minus 1/2
# > print out after every 100 tosses the number of heads minus half the number of tosses
# > do these numbers appear to approach 0 as n increases? 



# > 1.1) non-vectorized solution: -------------------------------------------

# function definition: 
coin_toss <- function(n){
    
    result <- c()  # empty vector that we'll fill via the loop 
    
    for (i in 1:n) {
        
        if (i == 1){
            # the first flip we just assign the toss result to variable "tosses"
            tosses <- sample(c(0,1), 1)
            
        } else {
            # creating a vector, "tosses" that has history of all tosses
            # We also fill this vector using the loop
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



# > 1.2) vectorized solution: ------------------------------------------------

# function definition: 
coin_toss_vectorized <- function(n, step = 100) {
    
    if (n < 100) {return(cat("n must be greater than 100"))}
    
    # Record num heads at each step
    tosses <- cumsum(sample(c(0, 1), n, replace = TRUE))
    
    # define vector of indices, based on step argument: 
    steps <- seq(step, n, by = step)
    
    # Compute summaries, indexing into vector "tosses", using the "steps" vector
    percent <- tosses[steps]/steps - .5
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



# > 1.3) Notes: ------------

cumsum  # function (x)  .Primitive("cumsum")

# The .Primitive above means that cumsum is implemented as a C function. 
# Reference: http://www.noamross.net/blog/2014/4/16/vectorization-in-r--why.html 

# In the **vectorized** solution, we first create a vector of sampled results, then
# **pass the entire vector to cumsum( ) to operate on in one go**.

# Also, calculations of percentages are vectorized: "tosses[steps]/steps" will
# return a vector, the first element of which will be the first element of
# tosses[steps] divided by the first element of steps, and so on

# In contrast, in the **non-vectorized** solution, at every stage, the operations
# only work with one coin toss at a time. Also, we are growing a vector through
# a for loop, which means there's overhear evey iteration of the loop, to
# allocate memory, etc.









#****************************************************************************
# Example 2: 2D Random walk ---------

# Reference: "Writing Efficient Programs in R (and Beyond)"
#****************************************************************************


# 2.1) non-vectorized solution: ---------------------------------------------

# define function: 
rw2d1 <- 
    function(n) {
        xpos = ypos = numeric(n)
        xdir = c(TRUE, FALSE)
        pm1 = c(1, -1)
        
        for(i in 2:n)
            if (sample(xdir, 1)) {
                xpos[i] = xpos[i-1] + sample(pm1, 1)
                ypos[i] = ypos[i-1]
            }
        else {
            xpos[i] = xpos[i-1]
            ypos[i] = ypos[i-1] + sample(pm1, 1)
        }
        data.frame(x = xpos,
                   y = ypos)
    }


# test function: 
rw2d1(10)

rw2d1(15000) %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_path(col = "dodgerblue3") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      
