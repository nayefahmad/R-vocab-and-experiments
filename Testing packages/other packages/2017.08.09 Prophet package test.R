
#****************************************
# TESTING PROPHET PACKAGE 
#****************************************

# install.packages("prophet")
# install.packages("Rcpp")
# install.packages("wikipediatrend")

library("dplyr")
library("curl")
library("devtools")
find_rtools()  # check whether RTools was installed properly
# TRUE 

#******************************************
library("prophet")  # why doesn't this work? 
# library("wikipediatrend")
Sys.getenv('PATH')  # check whether RTools is on the PATH. 
# Ans. NO 

# let's try adding rtools to the path: 
Sys.setenv("R_ZIPCMD" = "\\\\vch.ca/home$/HomeDir02/nahmad3/Tools/Rtools/bin") 
# ^ prob didn't work


#******************************************
library("rstan")
# check whether RStan is working: 
# define C function using inline package: 
library("inline")
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )                          
# error: status 127. This probably has something to do with the PATH 

# call function: 
fx( 2L, 5 ) # should be 10


