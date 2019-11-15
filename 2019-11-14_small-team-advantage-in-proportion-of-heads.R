
library(tidyverse)

small_team_wins <- function(n_trials,
                            small_team_size = 3, 
                            large_team_size = 20){
    
    # Vector of results for the small team. 
    # Each element is one simulated result. 
    small_team_prop_heads <- rbinom(n_trials, 
                                   small_team_size, 
                                   prob = 0.5)/small_team_size
    
    # Vector of results for the large team. 
    # Each element is one simulated result.
    large_team_prop_heads <- rbinom(n_trials, 
                                    large_team_size,
                                    prob = 0.5)/large_team_size
    
    # Out of all trials, how many did small team win?  
    small_team_wins_prop <- (sum(small_team_prop_heads > large_team_prop_heads))/n_trials
    
    return(small_team_wins_prop)
    
}


small_team_wins(10000)


data.frame(small_win_count = replicate(10000, 
                                       small_team_wins(n_trials = 1,
                                                       small_team_size = 6,
                                                       large_team_size = 100))) %>% 
    ggplot(aes(x = small_win_count)) + 
    geom_histogram(bins = 3)  


