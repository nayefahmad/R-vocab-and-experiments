

#***********************************************************
# MANAGING MANY MODELS WITH R 
#***********************************************************
# Created: 2018-04-21

# Talk by Hadley: https://www.youtube.com/watch?v=rz3_FDVt9eg 

library("gapminder")
library("tidyr")
library("purrr")
library("dplyr")
library("magrittr")
library("broom")
library("ggplot2")


# Notes: --------------------------------
# 1. tidyr is for nested data (dataframes within dataframes)
# 2. purrr is for functional programming (emphasize acions, not objects)
# 3. broom is for converting models to tidy data 


gapminder
str(gapminder)

# measure years since 1950: 
gapminder %<>%
      mutate(year1950 = year - 1950)

#**************************************************************************
# 1. Creating nested data using tidyr::nest( ): ------------------------------
#**************************************************************************

# use group by, then nest: 
by_country <- gapminder %>%
      group_by(country) %>% 
      nest()  # nest is from tidyr package 

by_country  
# 1 row for each country. All other cols of gapminder have been 
#     collapsed in to the data column of by_country. 
# Each row of by_country is for a single country
# Each entry of by_coutnry$data is a dataframe, which is nested 
#     within the by_country dataframe! 
# The "data" column is called a "list-column". Remember, a list 
#     can contain anything in R 

# str(by_country)  # str not helpful here 
by_country[1]  # not helpful 
by_country[[1]]  # not helpful ? 
by_country$data[[5]]



#**************************************************************************
# 2. Fitting models for each country using purrr::map( ) : ------------------------
#**************************************************************************
# fit lifexp ~ years.since.1950
# this will remove linear trend from the data 

# function for fitting regression: 
country_model <- function(df){
      lm(lifeExp ~ year1950, data =df) 
}


# create all the models, save in a new column, rename "by_country" to "models_by_country" : 
models_by_country <- by_country %>%
      mutate(model = map(data, country_model))  # data is the list-column in by_country. Each element is a df

models_by_country
models_by_country$model[[9]]

# > side note: map functions: -------------------
# map() is almost exactly the same as lapply()/sapply()
# take each element of data, and apply function country_model() to it

# map_lgl(), map_int(), map_dbl() and map_chr() return vectors of the corresponding type (or die trying).

# map2() takes 2 input vectors, applies a fn to take ith element 
# oth each of 2 inputs each time: 
output_length <- seq(10)
mu <- rnorm(10,20)

normals <- map2(output_length, mu, rnorm) %>% print


# what if you want to iteratively apply a function that takes more 
# than 2 inputs each time? 
# use pmap() : 
?pmap 
# pmap(.l, .f, ...)  
# .l is a list of lists. The length of .l determines the number of arguments that .f will be called with. List names will be used if present.

x <- list(1, 10, 100)
y <- list(1, 2, 3)
z <- list(5, 50, 500)

pmap(list(x, y, z), sum)


# you can even think of functions as data: give a list of fns
# that you want to apply to each col of a df: 
funs <- list(mean, median, sd)

funs %>% 
      map(~ mtcars %>% map_dbl(.x)) 
# todo: don't really understand this 
# todo: https://cran.r-project.org/web/packages/lazyeval/vignettes/lazyeval.html 

#*************************************************************













#**************************************************************************
# 3. Using broom package to convert models to tidy data: --------------------
#**************************************************************************

# broom::glance( )      ==> gives high level summary of model e.g. R.squared; 1 row per model
# broom::tidy( )        ==> gives model estimates e.g. coefficients in regression; 1 row per estimate
# broom::augment( )     ==> gives 1 row per observation for residuals, predicted values, etc. 

models_by_country %<>% 
      mutate(
            tidy = map(model, broom::tidy), 
            glance = map(model, broom::glance), 
            
            # let's specifically extract r squared: 
            rsq = glance %>% map_dbl("r.squared"),  # purr can be used to extract all objects with a certain name, here "r.squared"
            
            augment = map(model, broom::augment)
      )

models_by_country
models_by_country$tidy[[1]]
models_by_country$glance[[1]]
models_by_country$augment[[1]]  # new cols have "." in front to prevent conflicts with existing colnames  
models_by_country$rsq[[10]] 
models_by_country$rsq %>% hist   # most rsquared are very good 



# 4. Unnest to see details and plot: ---------------------
unnest(models_by_country, data)  # Recover initial data; note new col "rsq" 
unnest(models_by_country, model) # doesn't work
unnest(models_by_country, tidy)  # expand the "tidy" column

# > how many positive slopes vs negative: almost all positive: ------
unnest(models_by_country, tidy) %>% 
      filter(term == "year1950") %>% 
      select(estimate) %>% 
      ggplot(aes(x = estimate)) +
            geom_histogram() + 
            labs(title = "Histogram of slopes - change in lifeExp per year") + 
            theme_classic()
      
unnest(models_by_country, glance) %>% View  # view all high-level model summary stats 


# > let's plot all the rsquareds using the unnested data: -----
unnest(models_by_country, data) %>%
      ggplot(aes(rsq, reorder(country, rsq))) +   # todo: reorder( )?? 
      
      # stats::reorder(x, X) : 
      #     x is usually a factor 
      #     X is a vector of same length as x, whose subset of values
      #           for each unique level of x determines the eventual order of that level
      
      
      geom_point(aes(colour = continent)) + 
      theme_classic()


# > plotting slope vs intercept for all countries: ----------
models_by_country %>% 
      unnest(tidy) %>% 
      select(country, 
             term, 
             estimate, 
             rsq) %>% 
      
      # use tidyr::spread( ) to "spread a key-value pair across multiple columns" 
      # both Intercept and year1950 appear in the "term" col ==> we want 2 
      #     seperate cols for "Intercept" and "year1950" 
      # this is an alternative to reshape2::melt and dcast
      spread(term, estimate) %>%   # todo: this is an alternative to reshape??    
      ggplot(aes(x = `(Intercept)`, 
                 y = year1950)) +
      geom_point(aes(colour = rsq)) + 
      theme_classic()
      
      
      



