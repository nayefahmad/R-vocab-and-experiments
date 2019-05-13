

#****************************************************************************
# Vectorization examples 
# 2019-04-15
# Nayef 

#****************************************************************************

library(tidyverse)


# Example 1: Coin tosses -------------

# Reference: https://www.r-bloggers.com/how-to-use-vectorization-to-streamline-simulations/

# Write a program to: 
# > toss a coin n times and print out after every 100 tosses the proportion of
#   heads minus 1/2 
# > print out after every 100 tosses the number of heads minus half the number
#   of tosses 
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
    
    # return result
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

# In the **vectorized** solution, we first create a vector of sampled results,
# then **pass the entire vector to cumsum( ) to operate on in one go**.

# cumsum takes a vector as input, and **returns a vector the 
#   accumulates the input vector at every stage** 

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
# PDF: https://www.stat.auckland.ac.nz/~ihaka/downloads/Taupo.pdf 

#****************************************************************************


# > 2.1) non-vectorized solution: ---------------------------------------------

# define function: 
rw2d1 <- 
    function(n) {
        xpos = ypos = numeric(n)
        xdir = c(TRUE, FALSE)  # should we change xpos? If no, we'll change ypos  
        pm1 = c(1, -1)  # which direction to change xpos
        
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

# plot result: 
rw2d1(15000) %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_path(col = "dodgerblue3") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
      panel.grid.major = element_line(colour = "grey95"))
      




# > 2.2) First vectorized solution: ---------------------------------------------

# Rather than computing the position element by element, this
# version computes the vectors of position changes and then
# uses cumsum to compute the positions.

# To compute n positions we need n − 1 position changes.


# define function: 
rw2d2 <- function(n) {
    
    # Compute all step sizes: 
    steps <- sample(c(-1, 1), 
                    n-1, 
                    replace = TRUE) 
    
    # determine whether or not to step in x-direction: 
    xdir <- sample(c(TRUE, FALSE), 
                   n-1, 
                   replace = TRUE)
    
    # find full vector of xpos: 
    # if xdir = TRUE, we change xdir by amout "steps" (-1 or 1)
    
    # Vectorization: 
    # > ifelse is vectorized - applies on the whole xdir vector 
    # > cumsum is also vectorized 
    xpos <- c(0, cumsum(ifelse(xdir, 
                               steps, 
                               0)))
    
    # change ypos when you don't change xpos: 
    ypos <- c(0, cumsum(ifelse(xdir, 
                               0, 
                               steps)))
    
    # return result: 
    data.frame(x = xpos, 
               y = ypos)
    
}


# >> 2.2.1) Notes: -----
# note the importance of cumsum() in this example as well as
# in Example 1



# test function: 
set.seed(4); rw2d2(10)


# plot result: 
# plot result: 
rw2d2(15000) %>% 
    ggplot(aes(x=x, y=y)) + 
    geom_path(col = "dodgerblue3") + 
    theme_light() +
    theme(panel.grid.minor = element_line(colour = "grey95"), 
          panel.grid.major = element_line(colour = "grey95"))



# > 2.3) Second vectorized solution: ---------------------------------------------
# A potential problem with the previous version is the use of the
# ifelse function to deal with the x and y directions separately.
# As a final improvement let’s deal with the four step directions
# separately and simply choose one of the four directions at
# random.


