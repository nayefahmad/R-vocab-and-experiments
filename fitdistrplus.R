
#**************************************************
# fitting distributions with fitdistrplus 
#**************************************************

library("dplyr")
library("reshape2")
library("ggplot2")
library("fitdistrplus")  

# help(package="fitdistrplus")
# rm(list=ls())

#*********************************
# TODO: ---------------
# error: Error in as.graphicsAnnot(legend) : object 'plot.legend' not found
#*********************************



data(groundbeef)  # example dataset included in package 
str(groundbeef)

# plotting with plotdist(): 
plotdist(groundbeef$serving)
plotdist(groundbeef$serving, 
         histo=TRUE, 
         demp=TRUE)

# summary with descdist(): 
descdist(groundbeef$serving)  # produces Cullen-Frey graph 
descdist(groundbeef$serving, 
         boot=1000)  # 1000 bootstrap values? 


# fit a weibull dist: --------------
fweibull <- fitdist(groundbeef$serving, 
                    "weibull")
summary(fweibull)

# https://www.umass.edu/landeco/teaching/ecodata/schedule/likelihood.pdf
#  By convention, we usually minimize the negative log-likelihood function, but the solution is the same if we were to maximize the likelihood or log-likelihood functions

# fit gamma dist: ------------------
fgamma <- fitdist(groundbeef$serving, 
                  "gamma")
summary(fgamma)

# fit lognorm dist: ---------------
flnorm <- fitdist(groundbeef$serving, 
                  "lnorm")
summary(flnorm)


# plotting alternative distributions: -----------
candidate.dist <- list(fweibull, 
                       fgamma, 
                       flnorm)

denscomp(candidate.dist,
         legendtext = plot.legend)
# Error in as.graphicsAnnot(legend) : object 'plot.legend' not found

par(mfrow=c(2,2))

denscomp(candidate.dist)
qqcomp(candidate.dist)
cdfcomp(candidate.dist)
ppcomp(candidate.dist)

par(mfrow=c(1,1))






