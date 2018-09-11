

#************************************************************
# ESTIMATING A DENSITY FUNCTION FROM UNIVARIATE DATA 
#************************************************************

library("tidyverse")


# Example data: 
x <- c(0, 0, 0, 1, 5, 10)


# Estimate density using density( ) function: 
?density

x.dens.1 <- density(x)  # bandwidth automatically selected

# change the bandwidth to be narrower: 
x.dens.2 <- density(x, bw = .2)  # bw defines the bandwidth 


# plot the densities: 
plot(x.dens.1)
plot(x.dens.2)


# examine the density object: 
x.dens.1
names(x.dens.1)
str(x.dens.1)

# question: how to sample discrete random variables from this estimated density function? 
sample(x.dens.1$x, 100) 

# compare estimated density and sample from the estimated density: 
x.dens.1 %>% plot(xlim = c(-10, 20))
sample(x.dens.1$x, 
       prob = x.dens.1$y, 
       100000,
       replace = TRUE) %>% hist(xlim =c(-10, 20))



x.dens.2 %>% plot(xlim = c(-10, 20))
sample(x.dens.2$x,
       prob = x.dens.2$y, 
       100000, 
       replace = TRUE) %>% hist(xlim =c(-10, 20))


