
#'--- 
#' title: "stat_summary functions in ggplot"
#' author: "Nayef Ahmad"
#' date: "2019-11-03"
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
library(Hmisc)
library(broom)

#+ rest 

#' # Overview
#'
#' Reference:
#' https://stackoverflow.com/questions/19258460/standard-error-bars-using-stat-summary
#'
#' Let's look at the difference between 2 different ways of supplying functions
#' to `stat_summary`:
#'
#' 1. Binding the function (e.g. `mean`) to the argument `fun.y`
#'
#' 2. Binding the function (e.g. `mean_se` or `Hmisc::mean_cl_normal`) to the
#' argument `fun.data`
#'
#' This could be a good way to fit pointwise confidence intervals without
#' fitting a regression model to the entire dataset. 
#' 
 

# ?stat_summary

#' # Plot with 1 s.e. intervals around mean
#' 
#' This plot shows mean and 1 standard error intervals 

mtcars %>% 
    ggplot(aes(as.factor(cyl),
               qsec)) + 
    
    # stat_summary with arg "fun.y":
    # A function that returns a single number 
    stat_summary(fun.y = mean, 
                 geom = "point") + 
    
    # stat_summary with arg "fun.data": 
    # A function that is given the complete data and should return a data frame
    # with variables ymin, y, and ymax (for use in plotting ranges).
    
    # mean_se( ) is intended for use with stat_summary. It calculates mean and 
    # standard error 
    stat_summary(fun.data = mean_se,  
                 geom = "errorbar") + 
    
    scale_y_continuous(limits = c(10, 30))



#' # Plot with normal-based CI around mean
#' 
#' Now use function `mean_cl_normal` from package `{Hmisc}`. This gives the
#' sample mean and lower and upper Gaussian confidence limits based on the
#' t-distribution
 
# ?mean_cl_normal

#' Here's how the function works: 
#' 
smean.cl.normal

# plot 
mtcars %>% 
    ggplot(aes(as.factor(cyl),
               qsec)) + 
    
    # stat_summary with arg "fun.y":
    # A function that returns a single number 
    stat_summary(fun.y = mean, 
                 geom = "point") + 
    
    # stat_summary with arg "fun.data": 
    # A function that is given the complete data and should return a data frame
    # with variables ymin, y, and ymax (for use in plotting ranges).
    
    # mean_cl_normal( ) is intended for use with stat_summary. It calculates
    # sample mean and lower and upper Gaussian confidence limits based on the 
    # t-distribution
    stat_summary(fun.data = mean_cl_normal,  
                 geom = "errorbar") +
    
    scale_y_continuous(limits = c(10, 30))


#' # Why not regression?
#'
#' Consider the following. Is this a better way to show confidence intervals?
#'
#' **Ans.** No, not if cylinder is not actually a continuous variable.
#'
#' These are two different models:
#'
#' * if we treat `cyl` as continuous, then we estimate two parameters: <i> slope
#' and intercept </i>
#'
#' * if we treat `cyl` as factor, then we estimate 3 parameters: <i> intercept,
#' effect of `cyl = 6`, and effect of `cyl = 8` </i>
#'
#'
#'#' Note that the CI will not be displayed if you pass `cyl` as a factor (2nd
#' graph below)
#' 

# cyl as continuous
mtcars %>% 
    ggplot(aes(x = cyl,
               y = qsec)) + 
    geom_point() + 
    geom_smooth(method = "lm")


# cyl as factor
mtcars %>% 
    ggplot(aes(x = as.factor(cyl),
               y = qsec)) + 
    geom_point() + 
    geom_smooth(method = "lm")


#' # Exploring alternative models 

lm(qsec ~ cyl, 
   data = mtcars) %>% 
    tidy %>% 
    kableExtra::kable()


lm(qsec ~ as.factor(cyl), 
   data = mtcars) %>% 
    tidy %>% 
    kableExtra::kable()
