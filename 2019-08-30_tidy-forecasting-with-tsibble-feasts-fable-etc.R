
#'--- 
#' title: "Tidy forecasting with tsibble, feasts, fable, etc."
#' author: "Nayef Ahmad"
#' date: "2019-08-30"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---





library(magrittr)
library(ggplot2)
library(feasts)


# function for a random walk: 
random_walk <- function(t = 100,
                        drift = 0,
                        sd_white_noise = .5){
  
  # Note that rnorm returns a vector
  return(cumsum(drift + rnorm(t, mean = 0, sd = sd_white_noise)))
  
}



# simulate the random walk 
df1 <- data.frame(time = 1:100, 
                  value = random_walk())
df1 %>% 
  ggplot(aes(x = time, 
             y = value)) + 
  geom_line() + 
  scale_y_continuous(limits = c(-10, 10))



ts1 <- df1 %>% as_tsibble(index = time)




STL(ts1) %>% autoplot()




