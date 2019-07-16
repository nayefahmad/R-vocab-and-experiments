
#' ---
#' title: "Indexing a set of time series to compare % changes from baseline"
#' author: "Nayef Ahmad"
#' date: "2019-07-16"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---

#' 
#' ### References: 
#' 1. *Don't use dual-scaled y-axes* : https://www.perceptualedge.com/articles/visual_business_intelligence/dual-scaled_axes.pdf 
#' 2. *Simple guide to indexing* : https://www.dallasfed.org/research/basics/indexing.aspx 

#+ include = FALSE 
library(tidyverse)
library(kableExtra)

#' ### Original data 

#+ analysis 
df1.data <- 
    tibble::tribble(
        ~time, ~x1, ~x2,
            1,  25, 432,
            2,  28, 440,
            3,  32, 590,
            4,  20, 328,
            5,  40, 500,
            6,  44, 600,
            7,  42, 605,
            8,  50, 650,
            9,  55, 700,
           10,  60, 710
        )


df1.data %>% 
    kable() %>% 
    kable_styling(bootstrap_options = c("striped", 
                                        "responsive"), 
                  full_width = F, 
                  position = "left")

#' ### Plot without indexing 

df1.data %>% 
    gather(key = metric, 
           value = value, 
           -time) %>% 
    ggplot(aes(x = time, 
               y = value, 
               group = metric, 
               col = metric)) + 
    geom_line() + 
    labs(title = "Not very easy to compare growth rates", 
         subtitle = "Metrics have different scales which are not directly comparable")


#' ### Define function to index a column 

# function for indexing: 
index_function <- function(x){
    # input: x, a vector
    # output: x_indexed, a vector 
    
    initial <- x[1]
    
    x_indexed <- map_dbl(x, function(x){x/initial * 100})
    
    return(x_indexed)
    
}


# test the function: 
# x <- c(12, 24, 120, 1200)
# index_function(x)


#' ### Index the columns `X1` and `X2`

# now index the 2 series: 
df2.indexed <- 
    df1.data %>% 
    mutate(x1_indexed = index_function(x1), 
           x2_indexed = index_function(x2))


df2.indexed %>% 
    kable() %>% 
    kable_styling(bootstrap_options = c("striped", 
                                        "responsive"), 
                  full_width = F, 
                  position = "left")

#' ### Plot after indexing 

df2.indexed %>% 
    select(time, 
           x1_indexed, 
           x2_indexed) %>% 
    gather(key = metric, 
           value = value, 
           -time) %>% 
    ggplot(aes(x = time, 
               y = value, 
               group = metric, 
               col = metric)) + 
    geom_line() + 
    geom_hline(yintercept = 100, 
               col = "blue") + 
    labs(title = "Indexing the series makes growth rates easy to compare", 
         subtitle = "Each metric starts at 100, subsequent points show value as a % of initial value") 
    