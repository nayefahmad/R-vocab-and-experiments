

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

# expr( ) quotes YOUR expression (the formal argument of the function)
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
#   returns its formal argument. (todo: How is this useful?)


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









