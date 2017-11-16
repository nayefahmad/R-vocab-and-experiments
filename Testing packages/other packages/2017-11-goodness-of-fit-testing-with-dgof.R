
#*********************************
# DISTRIBUTION FITTING FOR ED VISITS AND ADMITS 
#*********************************

library("lubridate")
library("tidyr")
library("ggplot2")
library("dgof")

# rm(list=ls())


# reference distribution: 
ref.poisson <- rpois(1000, 8)
str(ref.poisson); summary(ref.poisson)
ggplot(data.frame(pois.rv=ref.poisson), aes(x=pois.rv)) + 
      geom_histogram(binwidth = 1)



# df with poisson means calculated from data 
df <- data.frame(pois.mean=c(70.5, 8.1, 18.2))

# generate a separate distribution for each row of df: 
pois.distributions <- lapply(df$pois.mean, rpois, n=1000) # %>% str

# function to plot all the distributions: 
plotdists <- function(vector.of.vars){
      hist(vector.of.vars)
}

# plot the distributions:  
setpar <- par(mfrow=c(length(pois.distributions), 1))
lapply(pois.distributions, plotdists)
par(setpar)



# test goodness-of-fit with reference dist: ---------

# try chi-square test: 
# chisq.test(table(pois.distributions[[1]], ref.poisson))
# chisq.test(pois.distributions[[2]], ref.poisson)
# ^^ chisq test not applicable for discrete distributions? 

# Cramer-von Mises test: 
# cvm.test(pois.distributions[[1]], ecdf(ref.poisson))
# cvm.test(pois.distributions[[2]], ecdf(ref.poisson))
# cvm.test(pois.distributions[[3]], ecdf(ref.poisson))
# 
# lapply(pois.distributions, cvm.test, y=ecdf(ref.poisson))


# Kolmogorov-Smirnov test: 
# H0: distributions are the same
# reject H0 if p value is small 
dgof::ks.test(pois.distributions[[1]], ecdf(ref.poisson))  # reject h0
dgof::ks.test(pois.distributions[[2]], ecdf(ref.poisson))  # accept H0
dgof::ks.test(pois.distributions[[3]], ecdf(ref.poisson))  # reject H0

# get results of above 3 lines at once: 
lapply(pois.distributions, ks.test, y=ecdf(ref.poisson))
