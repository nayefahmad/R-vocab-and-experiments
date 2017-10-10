
install.packages("prophet")
# install.packages("Rcpp")
# install.packages("wikipediatrend")

library("dplyr")
# library("DBI")
# library("Rcpp")
library("curl")
library("prophet")  # why doesn't this work? 
# library("prophet", lib="H:/R/R-3.4.1/library")  # why specify path for this?
# library("wikipediatrend")

library("devtools")
find_rtools()  # check whether RTools was installed properly
# TRUE 

Sys.getenv('PATH')  # check whether RTools is on the PATH

library("rstan")

# check whether RStan is working: 
fx <- inline::cxxfunction( signature(x = "integer", y = "numeric" ) , '
	return ScalarReal( INTEGER(x)[0] * REAL(y)[0] ) ;
' )                          
fx( 2L, 5 ) # should be 10


