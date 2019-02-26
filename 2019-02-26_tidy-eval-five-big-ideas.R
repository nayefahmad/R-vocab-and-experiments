

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


f1 <- function(hello){expr(hello)}
f1(a+b)  
#> hello 

# Q. Why does it return "hello"?
# Ans. Because "hello" is the name of the formal argument used in
#   the definition of the function. 

#   Regardless of what actual argument you pass to this function, it always just
#   returns its formal argument. 

# todo: How is this useful?
# Maybe in cases where you want a "strong default" value for an argument, 
#   where users can't change the value, but it's useful to be explicit that  
#   this is a fixed parameter


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

# example function:  
my_scatterplot <- function(df, xvar, yvar){
    
    # "quote"/"capture" the value that the user inputs (the actual arg),
    # but don't evaluate it: 
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





















