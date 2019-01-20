

#**********************************************
# Collatz conjecture calculator  
# 2019-01-20
# Nayef 

# references: 
# > https://en.wikipedia.org/wiki/Collatz_conjecture 
# > https://xkcd.com/710/ 

#**********************************************

library(tidyverse)

# function definition: --------
collatz_fn <- function(pos_integer){
    
    count <- 1 
    
    container <- vector()
    container[1] <- pos_integer
    
    while (pos_integer != 1){
        count <- count + 1 
        
        if (pos_integer %% 2 == 0){
            pos_integer <- pos_integer/2
            
        } else {
            pos_integer <- pos_integer*3 + 1
        }
        
        container[count] <- pos_integer
        
    }
    
    return(container)
      
}



# test the function: ---------------------
collatz_fn(3)

df <- collatz_fn(34) %>% 
    as.data.frame() %>% 
    mutate(x = 1:n()) %>% 
    set_names(c("result", "x"))  
    
df %>% 
    ggplot(aes(x = x, 
               y = result)) + 
    
    geom_line() + 
    geom_point() + 
    
    scale_x_continuous(expand = c(0, 0)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    
    labs(title = paste0("Starting number: ", 
                        df$result[1], 
                        "\nNum steps to get to 1: ", 
                        df$x[nrow(df)], 
                        " steps")) + 
    
    theme_classic(base_size = 12)







