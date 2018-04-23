

#************************************************************
# USING GGPLOT INSIDE FUNCTIONS
#************************************************************

library("dplyr")
library("magrittr")
library("ggplot2")




#***************************************************************
# 1. aes_string : ----------------------------------------------
#***************************************************************

# What if you don't want to specify ahead of time which cols will be on 
#     x and y axes? 

# fn to plot any 2 cols as scatter: 
plot_fn <- function(df, x.axis, y.axis) {
      ggplot(df, 
             aes(x = x.axis, 
                 y = y.axis)) + 
            
            geom_point()
}


# let's try out the fn: 
plot_fn(mtcars, "mpg", "cyl")
# this isn't what we wanted. ggplot is not looking at the mtcars dataset, it's just 
#     treating "mpg" and "cyl" as 2 strings


# fn with aes_string: 
plot.fn.with.aes <- function(df, x.axis, y.axis, point.colour = NULL) {
      ggplot(df, 
             aes_string(x = x.axis, 
                        y = y.axis, 
                        colour = point.colour)) + 
            
            geom_point()
}

# test the function: 
plot.fn.with.aes(mtcars, "mpg", "cyl")
plot.fn.with.aes(mtcars, "cyl", "qsec", "am")
plot.fn.with.aes(mtcars, "mpg", "cyl", "gear")
