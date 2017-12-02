

#****************************************
# Analysis of LOS data for 4E, 6E, 6W, 7E, IPS 
#****************************************

library("ggplot2")
library("dplyr")
library("fitdistrplus")

# rm(list=ls())


# Todo: -------------------
# > endpoints in subtitles of graphs: 2017-10-10? 
# > check KS statistics of dist. fitting 
# ******************************


# ****************************** 
# pull in clean data: 
source("2017-10-06_los-data-cleaning.R")
# str(losdata)

# pull in function to calculate LOS, extract arrival timestamps: 
source("los_function.R")
source("arrival-timestamp_function.R")




# ******************************
# Analysis for 4E ---------------
# ******************************

# > LOS calculation: --------------
# split data by unique encounter: 
split.losdata.4e <- split(losdata.4e, losdata.4e$id)
# str(split.losdata)

# apply los.fn, then combine results into a vector: 
los.4e <- 
      lapply(split.losdata.4e, los.fn, 
                 nursingunit = "4E")  %>%  # nursingunit is passed to los.fn
      unlist %>% unname 
str(los.4e)
summary(los.4e)


los.4e.df <- as.data.frame(los.4e)  # easier to work with in ggplot 
str(los.4e.df)

# tables of results: 
summary.4e <- 
      summarise(los.4e.df,
          unit="4E", 
          mean=mean(los.4e, na.rm=TRUE), 
          median=quantile(los.4e, probs = .50, na.rm=TRUE), 
          x90th.perc=quantile(los.4e, probs = .90, na.rm=TRUE))

table.4e <- table(los.4e) %>% as.data.frame

# >> fit LOS distribution: ----------
plotdist(los.4e)
descdist(los.4e[!is.na(los.4e)], boot=1000)  # gamma might be good fit

# remove NAs and negative values to fit gamma: 
los.4e.remove.na.neg <- los.4e[!is.na(los.4e)]
los.4e.remove.na.neg <- los.4e.remove.na.neg[!los.4e.remove.na.neg < 0]
summary(los.4e.remove.na.neg)

gamma.los.4e <- fitdist(los.4e.remove.na.neg, 
                        "gamma", 
                        method="mme")  
                        # ^ default mle estimation not working 
summary(gamma.los.4e)  # todo: why inf for log-likelihood, AIC, etc? 
parameters.los.4e <- summary(gamma.los.4e)$estimate
 

# # fit weibull: 
# wbull.los.4e <- fitdist(los.4e.remove.na.neg, 
#                         "weibull", 
#                         method="mme")

# fit lognormal: 
lgnorm.los.4e <- fitdist(los.4e.remove.na.neg, 
                         "lnorm")
summary(lgnorm.los.4e)

los.distributions <- list(gamma.los.4e, lgnorm.los.4e)

setpar <- par(mfrow=c(2,2))  # save current par before changing it
denscomp(los.distributions)  # not working? 
qqcomp(los.distributions)
cdfcomp(los.distributions)
ppcomp(los.distributions)
par(setpar)  # reset par


# try plotting the gamma and lnorm dist: 
# x <- seq(0.1, max(los.4e, na.rm=TRUE), .1)
# hx <- dgamma(x, shape = parameters.los.4e[1], rate=parameters.los.4e[2])
# gx <- dlnorm(x, meanlog = 1.637, sdlog = 1.265)
# 
# dist <- data.frame(x=x, value=c(hx, gx), 
#                    fn=rep(c("gamma", "lnorm"),
#                           each=length(hx)))
# 
# ggplot(dist, aes(x=x, y=value, col=fn)) + 
#       geom_point() + 
#       # facet_wrap(~fn) + 
#       scale_x_continuous(limits = c(0,85))

#******************************
# > Extract arrival timestamps: ------------
arrivals.4e <- lapply(split.losdata.4e, arrival.fn, 
                      nursingunit = "4E")
arrivals.4e <- do.call("c", arrivals.4e) %>% unname

str(arrivals.4e)
summary(arrivals.4e)
head(arrivals.4e)

# > Find interarrival times: ----
# first reorder the arrival timestamps 
arrivals.4e <- arrivals.4e[order(arrivals.4e)]

interarrivals.4e <- difftime(arrivals.4e, lag(arrivals.4e)) %>% 
      as.numeric(units="days") * (24*60)  # convert from days to minutes 

# combine in df: 
arrivals.4e.df <- data.frame(arrivals=arrivals.4e, 
                             inter=interarrivals.4e)

# >> fit distribution:------ 
plotdist(interarrivals.4e)  # looks exponential 
descdist(interarrivals.4e[!is.na(interarrivals.4e)],  # remove NA values 
         boot=1000)  # possibly beta??

# fit exponential dist: 
exp.arrival.4e <- fitdist(interarrivals.4e[!is.na(interarrivals.4e)],
                  "exp")
summary(exp.arrival.4e)  # rate parameter, lambda = 0.0033249 
lambda.arrival.4e <- summary(exp.arrival.4e)$estimate
# rpois(100000, lambda.4e*60) %>% hist
# ^ given this rate paramter for interarrivals (in minutes), we expect 
# the number of arrivals in 60 minutes to be between 0 and 3

# diagnostics: 
setpar <- par(mfrow=c(2,2))  # save current par before changing it
denscomp(exp.arrival.4e)
qqcomp(exp.arrival.4e)
cdfcomp(exp.arrival.4e)
ppcomp(exp.arrival.4e)
par(setpar)  # reset par 

gofstat(exp.arrival.4e)  # KS stat prob not in rejection region (??): 
                         # i.e. fail to reject exponential dist


# ******************************
# Analysis for 4W ---------------
# ******************************

# split data by unique encounter: 
split.losdata.4w <- split(losdata.4w, losdata.4w$id)

# apply los.fn, then combine results into a vector: 
los.4w <- 
      lapply(split.losdata.4w, los.fn, 
             nursingunit = "4W")  %>%  # nursingunit is passed to los.fn
      unlist %>% unname 
str(los.4w)
summary(los.4w)


los.4w.df <- as.data.frame(los.4w)  # easier to work with in ggplot 
str(los.4w.df)

# tables of results: 
summary.4w <- 
      summarise(los.4w.df,
                unit="4W", 
                mean=mean(los.4w, na.rm=TRUE), 
                median=quantile(los.4w, probs = .50, na.rm=TRUE), 
                x90th.perc=quantile(los.4w, probs = .90, na.rm=TRUE))

table.4w <- table(los.4w) %>% as.data.frame


# ******************************
# Analysis for 6E ---------------
# ******************************

# split data by unique encounter: 
split.losdata.6e <- split(losdata.6e, losdata.6e$id)
# str(split.losdata)

# apply los.fn, then combine results into a vector: 
los.6e <- 
      lapply(split.losdata.6e, los.fn, 
             nursingunit = "6E")  %>% 
      unlist %>% unname 
str(los.6e)
summary(los.6e)


los.6e.df <- as.data.frame(los.6e)  # easier to work with in ggplot 
str(los.6e.df)

# tables of results: 
summary.6e <- 
      summarise(los.6e.df,
                unit="6E", 
                mean=mean(los.6e, na.rm=TRUE), 
                median=quantile(los.6e, probs = .50, na.rm=TRUE), 
                x90th.perc=quantile(los.6e, probs = .90, na.rm=TRUE))

table.6e <- table(los.6e) %>% as.data.frame
# note: id=6660898079-17064865 has LOS = -1 because discharge date = 2017-05-29
#     and transfer date = 2017-05-30. Not sure why that would happen. 



# ******************************
# Analysis for 6W ---------------
# ******************************

# split data by unique encounter: 
split.losdata.6w <- split(losdata.6w, losdata.6w$id)
# str(split.losdata.6w)

# apply los.fn, then combine results into a vector: 
los.6w <- 
      lapply(split.losdata.6w, los.fn, 
             nursingunit = "6W")  %>% 
      unlist %>% unname 
str(los.6w)
summary(los.6w)


los.6w.df <- as.data.frame(los.6w)  # easier to work with in ggplot 
str(los.6w.df)

# tables of results: 
summary.6w <- 
      summarise(los.6w.df,
                unit="6W", 
                mean=mean(los.6w, na.rm=TRUE), 
                median=quantile(los.6w, probs = .50, na.rm=TRUE), 
                x90th.perc=quantile(los.6w, probs = .90, na.rm=TRUE))

table.6w <- table(los.6w) %>% as.data.frame



# ******************************
# Analysis for 7E ---------------
# ******************************

# LOS calculation:
# split data by unique encounter: 
split.losdata.7e <- split(losdata.7e, losdata.7e$id)
# str(split.losdata)

# apply los.fn, then combine results into a vector: 
los.7e <- 
      lapply(split.losdata.7e, los.fn, 
             nursingunit = "7E")  %>%  # nursingunit is passed to los.fn
      unlist %>% unname 
str(los.7e)
summary(los.7e)


los.7e.df <- as.data.frame(los.7e)  # easier to work with in ggplot 
str(los.7e.df)

# tables of results: 
summary.7e <- 
      summarise(los.7e.df,
                unit="7E", 
                mean=mean(los.7e, na.rm=TRUE), 
                median=quantile(los.7e, probs = .50, na.rm=TRUE), 
                x90th.perc=quantile(los.7e, probs = .90, na.rm=TRUE))

table.7e <- table(los.7e) %>% as.data.frame




# ******************************
# Analysis for IPS: ---------------
# ******************************

# LOS calculation:
# split data by unique encounter: 
split.losdata.ips <- split(losdata.ips, losdata.ips$id)
# str(split.losdata)

# apply los.fn, then combine results into a vector: 
los.ips <- 
      lapply(split.losdata.ips, los.fn, 
             nursingunit = "IPS")  %>%  # nursingunit is passed to los.fn
      unlist %>% unname 
str(los.ips)
summary(los.ips)


los.ips.df <- as.data.frame(los.ips)  # easier to work with in ggplot 
str(los.ips.df)

# tables of results: 
summary.ips <- 
      summarise(los.ips.df,
                unit="IPS", 
                mean=mean(los.ips, na.rm=TRUE), 
                median=quantile(los.ips, probs = .50, na.rm=TRUE), 
                x90th.perc=quantile(los.ips, probs = .90, na.rm=TRUE))

table.ips <- table(los.ips) %>% as.data.frame




# ******************************
# Plotting with ggplot: ------------
# ******************************

# > Graphs for 4E --------
p1_hist.4e <- 
      ggplot(los.4e.df, 
             aes(x=los.4e)) + 
      geom_histogram(stat="bin", 
                     binwidth = 1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,85), 
                         breaks=seq(0,85,5), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH 4E", 
           subtitle="From 2014-04-01 onwards \nMedian = 4.9 days; Mean = 11.1 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-10-10 ") + 
      
      geom_vline(xintercept = summary.4e[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.4e[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p1_hist.4e


# plot interarrival times: 
p5_inter.4e <- 
      ggplot(arrivals.4e.df, 
             aes(x=inter)) + 
      geom_histogram(stat="bin", 
                     binwidth = 60, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,3000), 
                         breaks=seq(0,3000,200), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="Interarrival time in min", 
           y="Number of cases", 
           title="Distribution of interarrival times in LGH 4E", 
           subtitle="From 2014-04-01 to 2017-10-10", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-10-10 ") + 
      
      theme_classic(base_size = 16); p5_inter.4e



# > Graph for 4W --------
p2_hist.4w <- 
      ggplot(los.4w.df, 
             aes(x=los.4w)) + 
      geom_histogram(stat="bin", 
                     binwidth = 1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,85), 
                         breaks=seq(0,85,5), 
                         expand=c(0,0)) + 
      scale_y_continuous(limits=c(0,200), 
                         expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH 4W", 
           subtitle="From 2014-04-01 onwards \nMedian = 18.8 days; Mean = 25.4 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-10-11 ") + 
      
      geom_vline(xintercept = summary.4w[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.4w[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p2_hist.4w


# > Graph for 6E --------
p3_hist.6e <- 
      ggplot(los.6e.df, 
             aes(x=los.6e)) + 
      geom_histogram(stat="bin", 
                     binwidth = 1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,85), 
                         breaks=seq(0,85,5), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH 6E", 
           subtitle="From 2014-04-01 onwards \nMedian = 2.9 days; Mean = 6.7 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-10-10 ") + 
      
      geom_vline(xintercept = summary.6e[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.6e[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p3_hist.6e


# > Graph for 6W: --------------
p4_hist.6w <- 
      ggplot(los.6w.df, 
             aes(x=los.6w)) + 
      geom_histogram(stat="bin", 
                     binwidth = 1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,85), 
                         breaks=seq(0,85,5), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH 6W", 
           subtitle="From 2014-04-01 onwards \nMedian = 3.0 days; Mean = 8.5 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-10-11 ") + 
      
      geom_vline(xintercept = summary.6w[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.6w[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p4_hist.6w




# > Graph for 7E: --------------
p6_hist.7e <- 
      ggplot(los.7e.df, 
             aes(x=los.7e)) + 
      geom_histogram(stat="bin", 
                     binwidth = 1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-1,85), 
                         breaks=seq(0,85,5), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH 7E", 
           subtitle="From 2014-04-01 onwards \nMedian = 3.5 days; Mean = 10.0 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-11-17") + 
      
      geom_vline(xintercept = summary.7e[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.7e[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p6_hist.7e



# > Graph for IPS: --------------
p7_hist.ips <- 
      ggplot(los.ips.df, 
             aes(x=los.ips)) + 
      geom_histogram(stat="bin", 
                     binwidth = .1, 
                     col="black", 
                     fill="deepskyblue") + 
      
      scale_x_continuous(limits=c(-.1,1), 
                         breaks=seq(0,1,0.1), 
                         expand=c(0,0)) + 
      scale_y_continuous(expand=c(0,0)) + 
      
      labs(x="LOS in days", 
           y="Number of cases", 
           title="Distribution of LOS in LGH IPS", 
           subtitle="From 2014-04-01 onwards \nMedian = 0.3 days; Mean = 0.3 days; ", 
           caption= "\nData source: DSDW ADTCMart; extraction date: 2017-11-17") + 
      
      geom_vline(xintercept = summary.ips[,2], 
                 col="red") + 
      
      geom_vline(xintercept = summary.ips[,3], 
                 col="red", 
                 linetype=2) + 
      
      theme_classic(base_size = 16); p7_hist.ips






# *******************************************
# Save outputs: ---------------

pdf(file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_los-histogram-2015-2017.pdf", 
      height= 8.5, width = 14) 
p1_hist.4e
p2_hist.4w
p3_hist.6e
p4_hist.6w
p6_hist.7e
p7_hist.ips
dev.off()

# single summary table: 
write.csv(rbind(summary.4e, 
                summary.4w, 
                summary.6e, 
                summary.6w, 
                summary.7e, 
                summary.ips),
          file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_LGH_summary-of-LOS.csv", 
          row.names = FALSE)


# histogram data, 4E: 
write.csv(table.4e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_LGH_histogram-table_4E.csv", 
          row.names = FALSE)

# histogram data, 4W: 
write.csv(table.4w, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_LGH_histogram-table_4W.csv", 
          row.names = FALSE)

# histogram data, 6E: 
write.csv(table.6e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_LGH_histogram-table_6E.csv", 
          row.names = FALSE)

# histogram data, 6W: 
write.csv(table.6w, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-10-11_LGH_histogram-table_6W.csv", 
          row.names = FALSE)

# histogram data, 7E: 
write.csv(table.7e, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_histogram-table_7E.csv", 
          row.names = FALSE)

# histogram data, IPS: 
write.csv(table.ips, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-17_LGH_histogram-table_IPS.csv", 
          row.names = FALSE)

#*****************************
# interarrivals, 4E: 
write.csv(arrivals.4e.df, file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-07_arrival-timestamps-and-interarrivals-4e.csv", 
          row.names = FALSE)

# interarrival dist graph: 
pdf(file="\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-07_interarrivals-4e.pdf", 
    height= 8.5, width = 14) 
p5_inter.4e
dev.off()

