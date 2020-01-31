

#***********************************************************************
# TIDY EVAL: THE 5 BIG IDEAS 
# 2019-02-26
# Nayef 

# Reference: https://www.youtube.com/watch?v=nERXS3ssntw


#***********************************************************************

library(tidyverse)

# rm(list = ls())

# 1) R code is a tree: ----------------
# Every expression in R has a hierarchical structure. 



# 2) You can "capture a tree" by **quoting** an argument: ----------

# > 2.1) expr( ) quotes YOUR expression (the formal argument of the function) -----
?rlang::expr

# Quotation is a mechanism by which an expression supplied as argument is
# captured by a function. Instead of seeing the value of the argument, the
# function sees the recipe (the R code) to make that value.


f1 <- function(hello){expr(hello)}  # hello is an expression here 
f1(a+b)  
#> hello 

# Q. Why does it return "hello"?
# Ans. Because "hello" is the expression ("recipe") passed as the formal argument
# in the definition of the function. Note that the expression is NOT evaluated: 
# if it were evaluated, there would be an error, as we have not bound any value 
# to the expression hello. 

#   Regardless of what actual expression you pass to this function, it always just
#   returns the expression that is its formal argument. 

# todo: How is this useful?
# Maybe in cases where you want a "strong default" value for an argument, 
#   where users can't change the value, but it's useful to be explicit that  
#   this is a fixed parameter
# 
# More generally, this can be useful when you capture an expression that you 
# might use later in several places? See section 3.1 below. 


# compare with: 
f2 <- function(hello){hello}
f2(a+b)
#> Error in f2(a + b) : object 'a' not found

# Q. Why this error? "Error in f2(a + b) : object 'a' not found" 

# Ans. Function f2 takes a single argument. We put in a+b as the argument, so it
#   needs to evaluate this to find the value of the argument. However, there is 
#   no defined value of a or b in the global environment, so we get an error. 

#   If you first assign values to a and b, then call f2, you won't get an error.
#   You'll just print the sum of the two.


a <- 10
b <- 20

f2(a+b)  # 30 
f1(a+b)  # hello 




# > 2.2) enexpr( ) quotes the USER'S expression (actual argument): -------
?rlang::enexpr

f3 <- function(z){enexpr(z)}

f3(a+b)  # a + b 

# Important: note that even if a and b are defined in the global env, f3 DOES
# NOT return the sum of a and b. That is, it doesn't return the "cake", but 
# just the "recipe" for the cake. 

# This is useful when we want to allow the user to supply a recipe that can 
# later be used for something else. 


# > 2.3) "Evaluating" arguments versus "Quoting" arguments: -------
x <- rnorm(10); y <- rpois(10, 10)
mean(x + y)  # arguments are evaluated 

ggplot(mtcars, 
       aes(disp, mpg)) +  # these 2 arguments are quoted 
    geom_point()







# 3) "Unquoting" makes it possible to build complex trees -----------

# let's capture the expression "x + y", without evaluating it: 
xy_quoted <- expr(x + y)


# Now we can unquote with the "bang bang" operator ("!!"), along with expr()
# Before "!!", xy_quoted is "captured" or "frozen in time" - you can't use it 
# to combine with other things in more complex expressions. 

# The "!!" operator "unfreezes" the captured expression so that it can be 
# combined with other expressions. 

expr(!!xy_quoted)  # x + y

expr(!!xy_quoted + z)  # x + y + z

expr(1/!!xy_quoted)  # 1/(x + y)


# > 3.1) example: ----------

# create an expression, save to x
x <- expr(a + b)

# create a simple function: 
f4 <- function(){print("done")}


# "import" the expression into the call to function f: 
expr(f(!!x, y))  # f(a + b, y)


# 4) QUOTING AND UNQUOTING TOGETHER: -----------

# If you want to wrap a function that quotes one or more of its arguments, you 
# need to quote one or more of your arguments, then you need to unquote them. 

# This is a simple but powerful pattern if we want to wrap around any function 

# Note: in general, for capturing a singe variable, enquo() seems to be preferable,
# as it also captures environment (see below). 

# enexpr() is preferred when you have to capture an *expression*, such as `a + b`

# example function:  
my_scatterplot <- function(df, xvar, yvar){
    
    # "quote"/"capture" the value that the user inputs (the expression in the actual 
    # arg), but don't evaluate it: 
    xvar <- enexpr(xvar)  
    yvar <- enexpr(yvar)
    
    
    # "unquote" the quoted expression and pass to ggplot: 
    ggplot(df, 
           aes(!!xvar, !!yvar)) + 
        geom_point(colour = "red")
    
}


# test the fn: 
my_scatterplot(mtcars, mpg, cyl)
my_scatterplot(mtcars, mpg, hp)

# withough quote/unquote pattern, the above calls would fail because there are 
# no vars called "cyl" or "hp" or "mpg" in the global env. 

# What we're saying is: "hey, take this expression (e.g. "mpg"), and just 
# HOLD ON FOR NOW. Dont' evaluate it, UNTIL you're inside of a ggplot call. 
# ggplot will know how to deal with the expression, so don't do anything until then" 

# another example: 
my_summary <- function(df, var){
    var <- enexpr(var)
    summarise(df,
              mean(!!var), 
              quantile(!!var, .5), 
              quantile(!!var, .90)
              ) 
    
}

# test: 
my_summary(mtcars, mpg)
my_summary(mtcars, hp)






# 5) QUOSURE = CLOSURE + QUOTATION: ----------------

# This is useful in cases where variables are defined in different environments
# and we want to make sure we're getting the value from the right environment (?)

# Quosures capture both the expression AND THE ENVIRONMENT in which they should
# be evaluated.

# In general, it seems like the quosure created by enquo() is always preferable
# to using enexpr(). Todo: Is this true? 

# References: 
# > https://adv-r.hadley.nz/evaluation.html#quosures
# > https://laderast.github.io/2017/12/19/understanding-tidyeval/


# Here's an example of a problem that was fixed using a quosure: 
# https://github.com/business-science/tibbletime/pull/83/commits/b3f1a9a492cefcaa1bf70cca1ac5360d789a816a

# The issue was in the {tibbletime} package, where the collapse_by( ) function
# would fail if user had a df with a colname `start_date`


?rlang::enquo


# example 1:  
my_scatterplot_enquo <- function(df, xvar, yvar){
    
    # "quote"/"capture" the value that the user inputs (the actual arg),
    # but don't evaluate it: 
    xvar <- enquo(xvar)  # you can also use enexpr( ), but this seems preferable
    yvar <- enquo(yvar)
    
    
    # "unquote" the quoted expression and pass to ggplot: 
    ggplot(df, 
           aes(!!xvar, !!yvar)) + 
        geom_point(colour = "red")
    
}


# test the fn: 
my_scatterplot_enquo(mtcars, mpg, cyl)
my_scatterplot_enquo(mtcars, mpg, hp)



# example 2
# try a that replicates dplyr::select(): 
grab_col <- function(df, col_required){
    
    col_required <- enquo(col_required)
    
    df %>% pull(!!col_required)
}

# test the fn: 
mtcars %>% grab_col(mpg)


