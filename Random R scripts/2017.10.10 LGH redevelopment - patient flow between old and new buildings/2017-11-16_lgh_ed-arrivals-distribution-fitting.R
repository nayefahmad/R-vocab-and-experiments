
#*********************************
# DISTRIBUTION FITTING FOR ED VISITS AND ADMITS 
#*********************************

library("lubridate")
library("tidyr")
library("ggplot2")
library("dgof")
library("xlsx")
library("readxl")



# rm(list=ls())

#**********************************
# Example: using ks.test() function to test goodness-of-fit --------- 
#**********************************
#2017-11-22_ED_Number_of_Visits_by_Hour.csv
#2017-11-22_4E_Direct_Admits_by_Hour.csv
#2017-11-22_6E_Direct_Admits_by_Hour.csv
#2017-11-22_6W_Direct_Admits_by_Hour.csv
#2017-11-22_7E_Direct_Admits_by_Hour.csv
#2017-11-22_IPS_Direct_Admits_by_Hour.csv
CSVDataFile <- "G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-22_ED_Number_of_Visits_by_Hour.csv"
ref.poisson <- read.csv(file = CSVDataFile, sep=",", header=TRUE)


str(ref.poisson); summary(ref.poisson)
ggplot(data.frame(pois.rv=ref.poisson), aes(x=pois.rv)) + 
      geom_histogram(binwidth = 1)


#2017-11-22_ED_Average_Number_of_Visits_by_Hour.csv
#2017-11-22_4E_Average_Direct_Admits_by_Hour.csv
#2017-11-22_6E_Average_Direct_Admits_by_Hour.csv
#2017-11-22_6W_Average_Direct_Admits_by_Hour.csv
#2017-11-22_7E_Average_Direct_Admits_by_Hour.csv
#2017-11-22_IPS_Average_Direct_Admits_by_Hour.csv

ED_Average <- read.csv ("G:/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.10.10 LGH redevelopment - patient flow between old and new buildings/results/output from src/2017-11-22_ED_Average_Number_of_Visits_by_Hour.csv", header = TRUE, sep = ",")
#df <- data.frame(pois.mean <-read.table(file = CSVAVg))


pois.mean = rep(0,24)
for (tempCounter in 1:24){ pois.mean[tempCounter] <- ED_Average[1, tempCounter]}
df <- data.frame(pois.mean)

# generate a separate distribution for each row of df: 
pois.distributions <- lapply(df$pois.mean, rpois, n=1000) # %>% str

# plot the distributions:  
setpar <- par(mfrow=c(4,6))
lapply(pois.distributions, hist)
par(setpar)

# > test goodness-of-fit with reference dist: ---------
# Kolmogorov-Smirnov test: 
# H0: distributions are the same
# reject H0 if p value is small 
dgof::ks.test(pois.distributions[[1]], ecdf(ref.poisson[ , 1]))  # accept H0
dgof::ks.test(pois.distributions[[2]], ecdf(ref.poisson[ , 2]))  # accept H0
dgof::ks.test(pois.distributions[[3]], ecdf(ref.poisson[ , 3]))  # accept H0
dgof::ks.test(pois.distributions[[4]], ecdf(ref.poisson[ , 4]))  # accept H0
dgof::ks.test(pois.distributions[[5]], ecdf(ref.poisson[ , 5]))  # accept H0
dgof::ks.test(pois.distributions[[6]], ecdf(ref.poisson[ , 6]))  # accept H0
dgof::ks.test(pois.distributions[[7]], ecdf(ref.poisson[ , 7]))  # accept H0
dgof::ks.test(pois.distributions[[8]], ecdf(ref.poisson[ , 8]))  # accept H0
dgof::ks.test(pois.distributions[[9]], ecdf(ref.poisson[ , 9]))  # accept H0
dgof::ks.test(pois.distributions[[10]], ecdf(ref.poisson[ , 10]))  # accept H0 
dgof::ks.test(pois.distributions[[11]], ecdf(ref.poisson[ , 11]))  # accept H0
dgof::ks.test(pois.distributions[[12]], ecdf(ref.poisson[ , 12]))  # accept H0
dgof::ks.test(pois.distributions[[13]], ecdf(ref.poisson[ , 13]))  # accept H0
dgof::ks.test(pois.distributions[[14]], ecdf(ref.poisson[ , 14]))  # accept H0
dgof::ks.test(pois.distributions[[15]], ecdf(ref.poisson[ , 15]))  # accept H0
dgof::ks.test(pois.distributions[[16]], ecdf(ref.poisson[ , 16]))  # accept H0
dgof::ks.test(pois.distributions[[17]], ecdf(ref.poisson[ , 17]))  # accept H0
dgof::ks.test(pois.distributions[[18]], ecdf(ref.poisson[ , 18]))  # accept H0
dgof::ks.test(pois.distributions[[19]], ecdf(ref.poisson[ , 19]))  # accept H0
dgof::ks.test(pois.distributions[[20]], ecdf(ref.poisson[ , 20]))  # accept H0
dgof::ks.test(pois.distributions[[21]], ecdf(ref.poisson[ , 21]))  # accept H0
dgof::ks.test(pois.distributions[[22]], ecdf(ref.poisson[ , 22]))  # accept H0 
dgof::ks.test(pois.distributions[[23]], ecdf(ref.poisson[ , 23]))  # accept H0
dgof::ks.test(pois.distributions[[24]], ecdf(ref.poisson[ , 24]))  # accept H0

# get results of above 3 lines at once: 
#lapply(pois.distributions, ks.test, y=ecdf(ref.poisson))

# > plot distributions: ----
setpar <- par(mfrow=c(1, 2))
hist(ref.poisson[ , 11])
hist(pois.distributions[[11]])
par(setpar)
