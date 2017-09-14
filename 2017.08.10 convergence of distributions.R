

library("dplyr")
library("reshape2")
library("lazyeval")
library("ggplot2")
library("labeling")
library("lubridate")


# help(package="base")
# rm(list=ls())

# create input vector: 
x <- seq(-4,4, length=100)

# ---------------------
# plot pdfs: --------------
# ---------------------

fx <- function(n, input_vector){
      dnorm(x, mean=0, sd=1/sqrt(n))
}

# fx1 <- fx(1,input_vector = x)

# create data frame with x and f(x) values for different values of n: 
normal_curves <- lapply(seq(1:10), fx, input_vector=x) %>% 
      # "input_vector=x" => specify that second arg of fx always takes value x
      as.data.frame

colnames(normal_curves) <- paste0("n", 1:10)
normal_curves <- mutate(normal_curves,
                        x=seq(-4,4, length=100)) %>%
                  select(11,1:10)

str(normal_curves)


# putting data in long form to allow facetting by value of n: 
normal_curves_long <- melt(normal_curves, id.vars = "x")
str(normal_curves_long)

p1 <- ggplot(normal_curves_long, aes(x=x, y=value, group=variable)) + 
      geom_line() + 
      geom_text(data=filter(normal_curves_long,
                            x>=1 & x<=1.05),  # filtering to get single point from each                                                  curve 
                aes(label=variable), 
                col="red") ; p1  
      
            
            
# ---------------------
# plot cdfs: --------------
# ---------------------
Fx <- function(n, input_vector){
      pnorm(x, mean=0, sd=1/sqrt(n))
}

# fx1 <- fx(1,input_vector = x)

# create data frame with x and f(x) values for different values of n: 
normal_curves_cdf <- lapply(seq(1:10), Fx, input_vector=x) %>% 
      # "input_vector=x" => specify that second arg of fx always takes value x
      as.data.frame

colnames(normal_curves_cdf) <- paste0("n", 1:10)
normal_curves_cdf <- mutate(normal_curves_cdf,
                        x=seq(-4,4, length=100)) %>%
      select(11,1:10)

str(normal_curves_cdf)
summary(normal_curves_cdf)

# putting data in long form to allow facetting by value of n: 
normal_curves_cdf_long <- melt(normal_curves_cdf, id.vars = "x")
str(normal_curves_cdf_long)

p2 <- ggplot(normal_curves_cdf_long, aes(x=x, y=value, group=variable)) + 
      geom_line() + 
      geom_text(data=filter(normal_curves_cdf_long,
                            x>=.5 & x<=.6),  # filtering to get single point from each                                                  curve 
                aes(label=variable), 
                col="red") ; p2  


      
