
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

# plotting with plotdist(): -----------
plotdist(groundbeef$serving)
plotdist(groundbeef$serving, 
         histo=TRUE, 
         demp=TRUE)

# summary with descdist(): ------------
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

par(mfrow=c(2,2))  # can use "setpar <- par(..)" then later "par(setpar)" to reset par

denscomp(candidate.dist)
qqcomp(candidate.dist)
cdfcomp(candidate.dist)
ppcomp(candidate.dist)

par(mfrow=c(1,1))

# statistical goodness of fit tests: 
gofstat(candidate.dist)




#*********************************
# ANOTHER EXAMPLE ED INTERARRIVALS: ----------
#*********************************

ed.start.times <- read.csv("H:/VCH files - Nayef/R-vocab-and-experiments/data/Lunch&Learn data.csv", 
                  stringsAsFactors = FALSE)

names(ed.start.times) <- tolower(names(ed.start.times))
ed.start.times$starttime <- strptime(ed.start.times$starttime,
                                     format="%R")

str(ed.start.times)
ed.start.times$starttime

interarrival <- diff(ed.start.times$starttime) %>% as.numeric
str(interarrival)


# plotting with plotdist() --------
plotdist(interarrival, 
         histo=TRUE, 
         demp=TRUE)

# description with descdist() ----------
descdist(interarrival)  # suggests gamma, exp and beta are possibilities  
descdist(interarrival, 
         boot=1000)  # suggests beta is most likely?? 


# fit an exponential distribution: ----------

# fexp <- fitdist(interarrival, 
#                     "exponential")  
# above: doesn't work because there's no function dexponential defined

fexp <- fitdist(interarrival,
                    "exp")
summary(fexp)


# fit a beta dist -------------
# fbeta <- fitdist(interarrival, 
#                  "beta")  
# weird error. see https://stackoverflow.com/questions/44507568/error-when-fitting-a-beta-distribution-the-function-mle-failed-to-estimate-the 

interarrival.scaled <- (interarrival - min(interarrival) + 
                              0.001) / (max(interarrival) - 
                                              min(interarrival) + 0.002)
fbeta <- fitdist(interarrival.scaled,
                 "beta")
summary(fbeta)

# plotting beta and exp dist: -------------
interarrival.candidate.dist <- list(fexp,fbeta)

par(mfrow=c(2,2))

denscomp(interarrival.candidate.dist)
qqcomp(interarrival.candidate.dist)
cdfcomp(interarrival.candidate.dist)
ppcomp(interarrival.candidate.dist)

par(mfrow=c(1,1))


# you can use ecdf() and density to get empirical distributions: 
ecdf(interarrival) %>% plot 
density(interarrival) %>% plot 
