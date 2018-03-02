

#*******************************************************************
# TESTING PACKAGE BROMAN 
#*******************************************************************

library("broman")
library("magrittr")

help(package="broman")


#*******************************************************************
# T-TESTS IN R: --------
# > I'm most interested in fn broman::paired.perm.test( )
# > but first let's see how stats::t.test( ) works








#*******************************************************************
# PAIRED T-TESTS USING PAIRED.PERM.TEST: ------
# > example 1: ----
x <- c(43.3, 57.1, 35.0, 50.0, 38.2, 31.2)
y <- c(51.9, 95.1, 90.0, 49.7, 101.5, 74.1)

d <- x-y  # vector of differences 

paired.perm.test(d, pval=FALSE)  # full results 
paired.perm.test(d, pval=TRUE)  # just the pval 

# If pval=TRUE, the output is a single number: the P-value testing for the symmetry about 0 of the distribution of the population from which d was drawn. 
# If pval=FALSE, the output is a vector of the t statistics from the permutations.



# > example 2: -------
x <- rnorm(30, 200)  # %>% print 
y <- rnorm(30, 190)  # %>% print 

d <- x-y

paired.perm.test(d, pval=TRUE)  
# Error in binary.v(n) : cannot allocate vector of length 1073741824
# > too many possible permutations? 

paired.perm.test(d, n.perm=10000, pval=TRUE)  

#*******************************************************************