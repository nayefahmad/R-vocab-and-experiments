

#'--- 
#' title: "The fundamental component of the normal distribution"
#' author: "Nayef Ahmad"
#' date: "2019-10-07"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float: true
#' ---
#' 

#+ lib, include = FALSE 
library(tidyverse)

#' At first glance, the [probability density function (pdf) of the normal
#' distribution](https://sites.nicholas.duke.edu/statsreview/files/2013/06/normpdf1.jpg) has a lot going on. At its heart, though, is a fairly simple
#' function: $e^{(-x^2)}$
#'
#' Let's see how we can go from this simple building block to something closer
#' to the actual normal distribution.
#' 

#' # The "proto-normal" distribution: `exp(-(x^2))` 
#' Define the function: 
exp_fun <- function(x){
  exp(-(x^2))
}


#' Plot it: 
#' 
data.frame(x = c(-5, 5)) %>% 
  ggplot(aes(x = x)) + 
  stat_function(fun = exp_fun) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

#' By default, the graph will be centered around zero, and will have a certain
#' "natural" variance
#' 

#' # Parametrizing the centre and spread of the graph
#'
#' We'll now make two simple changes that will allow us to vary the centre and
#' the spread of the graph
#' 
#' 

exp_fun_with_mean_var <- function(x, mu = 5, var = 1){
  exp(-((x-mu)^2/2*var^2))
}

#' Let's look at graphs with the same centre and different spreads: 

data.frame(x = c(0, 10)) %>% 
  ggplot(aes(x = x)) + 
  stat_function(fun = exp_fun_with_mean_var, 
                args = list(mu = 5, 
                            var = 1)) + 
  stat_function(fun = exp_fun_with_mean_var, 
                args = list(mu = 5, 
                            var = 2)) + 
  stat_function(fun = exp_fun_with_mean_var, 
                args = list(mu = 5, 
                            var = 3)) + 
  theme_light() +
  theme(panel.grid.minor = element_line(colour = "grey95"), 
        panel.grid.major = element_line(colour = "grey95"))
      

#' Note that the curve with maximum spread is an **envelope around all the other curves**. 
#' This is not how actual normal distributions behave - see [this page](http://dpuadweb.depauw.edu/harvey_web/Data_Analysis/Pages/investigation2225.html) 
#' for example. The areas under all of these curves cannot all be equal (and 
#' hence they can't all be 1.0). Thus, these don't work as probability density functions. 
#' 
#' This is a hint that this simple exponential has to be **scaled** somehow 
#' in order to impose the structure we want. 
#' 
#' For comparison, here are the normal distributions with those parameters: 
#' 
#' TODO: 





