

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


# Notes: --------------------------------
# 1. tidyr is for nested data (dataframes within dataframes)
# 2. purrr is for functional programming (emphasize acions, not objects)
# 3. broom is for converting models to tidy data 


gapminder
str(gapminder)

# measure years since 1950: 
gapminder %<>%
      mutate(year1950 = year - 1950)


# nested data: ------------------------------

# use group by, then nest: 
by_country <- gapminder %>%
      group_by(country) %>% 
      nest()  # nest is from tidyr

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




# fitting models for each country: ------------------------
# fit lifexp ~ years.since.1950
# this will remove linear trend from the data 

# function for fitting regression: 
country_model <- function(df){
      lm(lifeExp ~ year1950, data =df) 
}


# create all the models, save in a new column: 
models <- by_country %>%
      mutate(model = map(data, country_model))



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
