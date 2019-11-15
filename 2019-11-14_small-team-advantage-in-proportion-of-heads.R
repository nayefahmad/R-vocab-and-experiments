
#'--- 
#' title: "Coin flipping game: Are small teams at an advantage or disadvantage?"
#' author: "Nayef Ahmad"
#' date: "2019-11-14"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#'     toc_folding: false
#' ---
#' 
#' 

#+ lib, include = FALSE 
library(tidyverse)

#+ rest 
#' # Problem statement 
#' 

#' Two teams play the "coin flipping game". There is a small team that has 3
#' players, and a large team that has 20 players.
#' 

#' Here are the rules: 
#' 
#' 1. Each player flips a coin
#' 
#' 2. We count the total number of heads that a team gets
#' 
#' 3. Divide the total number of heads by the team size, to get the proportion of heads 
#' 
#' 4. The team with the higher proportion of heads wins the game
#' 
#' 
#' The question we are investigatins is: **is the smaller team at an advantage or a disadvantage in this game?**
#' 
#' We know, of course, that smaller samples give more highly variable results. 
#' See [Kahneman and Tversky's example about babies' gender proportions](https://en.wikipedia.org/wiki/Insensitivity_to_sample_size), for example.
#' 
#' However, in this case, I'm not sure that this higher variability will be either 
#' beneficial or detrimental for the smaller team. 
#' 
#' Let's simulate to find out! 
#' 


#' # Simulation setup 
#' 
#' We'll define a function to return a vector with the proportion of heads for each 
#' iteration of the game. 
#' 

# fn definition
head_prop_sims <- function(n_sims,
                           team_size){
    
    # n_sims: how many simulations are we running? 
    # team_size: this is the parameter n for the binomial random variable generator
    
    # example: 
    # for the first simulated result, the proportion of heads 
    # will be a binomial r.v. with parameters n = team_size, p = 0.5 
    
    # same for the second simulated result, and so on. 
    
    # Vector of results:  
    return(rbinom(n_sims, 
                  team_size, 
                  prob = 0.5)/team_size)
}

# fn test 
# head_prop_sims(10, 2)


#' # Analysis
#' 
#' Using our function with two different values for `team_size`, we get 
#' simulated heads proportions for both the small and large teams. 
#' 
#' 

n_iterations <- 100000
small_team_size <- 3
large_team_size <- 20

small_team_proportions <- head_prop_sims(n_iterations, small_team_size)
large_team_proportions <- head_prop_sims(n_iterations, large_team_size)


#' Next, we take the difference to see who wins each iteration. 
#' 
small_team_advantage <- small_team_proportions - large_team_proportions

#' Finally, let's plot the difference. If the difference is centred around zero, 
#' then team size has no impact on win probability. 
#' 


data.frame(small_team_advantage = small_team_advantage) %>% 
    ggplot(aes(x = small_team_advantage)) + 
    geom_histogram() + 
    labs(title = "Distribution of the difference in proportion of Heads", 
         subtitle = sprintf("Each data point is the difference between the proportion of Heads \nthat the small team got and the proportion of Heads that the large team got \n\nBased on %i iterations", 
                            n_iterations))


#' # Conclusion
#' 
#' It doesn't look like team size has an impact on win probability. 
#' 