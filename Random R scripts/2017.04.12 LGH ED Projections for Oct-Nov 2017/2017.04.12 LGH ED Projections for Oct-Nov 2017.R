

library("dplyr")
library("ggplot2")
library("reshape2")



# help(package="base")
# rm(list=ls())

## TODO: -------------------------------------------------------
# > print boxplots for weekends and weekdays 
# ------------------------------------------------------------


# READ IN FINAL DATA FROM SQL: -------------------
setwd("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.04.12 LGH ED Projections for Oct-Nov 2017")
data.orig <- read.csv("2017.04.12 LGH ED Projections for Oct-Nov 2017 - Raw data v1.csv")  
# filename: "2017.04.12 LGH ED Projections for Oct-Nov 2017 - Raw data v1" 

data <- data.orig  # work with copy for safety 

data$StartDate <- as.Date(as.character(data$StartDate), format= "%Y-%m-%d")
# str(data)
# summary(data)


# DATA WRANGLING: --------------------------------
data_grouped <- group_by(data, StartDate) %>% 
      summarize(num_cases=sum(num_cases))
# str(data_grouped)  
# head(data_grouped)

data_grouped$cst_period_identifier <- substr(data_grouped$StartDate, 6,10)
data_grouped$year <- as.character(substr(data_grouped$StartDate, 1,4))

# str(data_grouped)  
# summary(data_grouped)
# head(data_grouped)

# define Oct-Nov as CST season, in order to be able to filter these rows 
cst_season <- substr(seq(ISOdate(2010,10,1), ISOdate(2010,11,30), by="day"), 6,10) 
# data_grouped$cst_period_identifier[1:61] %in% cst_season
# data_grouped$cst_period_identifier[1:62] %in% cst_season

# grouped dataset covering CST period only: 
data_grouped_cst <- as.data.frame(filter(data_grouped, cst_period_identifier %in%
                                           cst_season))
str(data_grouped_cst)


# split CST-PERIOD data by year: ------------------------------
data_grouped_cst_by_year <- split(data_grouped_cst, data_grouped_cst$year)
# str(data_grouped_cst_by_year)

# data_grouped_cst_by_year[7] %>% as.data.frame %>% select(2)

result <- vector(mode="list")  # FOR SOME REASON THIS IS THE BEST WAY TO 
  # CREATE AN EMPTY CONTAINER TO LATER FILL WITH A LOOP. Don't use 
  # result <- list() or data.frame() or vector(). 
  # you can fill this with things of different lengths, data types (after converting to list?)! 
for(i in 1:7){
  result[i] <- data_grouped_cst_by_year[i] %>% as.data.frame %>% select(2)  # only include num_cases column (col num=2) in result
}
names(result) <- paste0("year_", 2010:2016)
str(result); summary(result); head(result)


result <- as.data.frame(result)
str(result)
# result


# ****************************************
# Plotting results: ----------
p1_histograms <- 
      ggplot(melt(result), aes(x=value)) + 
      geom_histogram(stat="bin") + 
      facet_wrap(~variable); p1_histograms

p2_boxplot <- 
      ggplot(melt(result), 
             aes(x=variable, y=value)) + 
      geom_boxplot() + 
      theme_classic(); p2_boxplot

# ****************************************



# Add weekdays for each year to result -----------------------
weekdays.df <- vector(mode="list")
for(i in 2010:2016){
  weekdays.df[i-2009] <- list(weekdays(seq(ISOdate(i,10,1), 
                                           ISOdate(i,11,30), by="day")))
}

weekdays.df <- as.data.frame(weekdays.df)
yearnames <- sapply(2010:2016, function(x) paste0("wkdays_", x))
names(weekdays.df) <- yearnames
weekdays.df <- cbind(cst_season, weekdays.df)
str(weekdays.df); head(weekdays.df)

# final df: 
final.df <- data.frame(weekdays.df[1], 
                       result[1], weekdays.df[2], 
                       result[2], weekdays.df[3],
                       result[3], weekdays.df[4],
                       result[4], weekdays.df[5],
                       result[5], weekdays.df[6],
                       result[6], weekdays.df[7],
                       result[7], weekdays.df[8])

str(final.df)


# select weekdays only: ------------
# filter(final.df, !(wkdays_2010 %in% c("Saturday", "Sunday"))) %>% 
#   select(cst_season, year_2010, wkdays_2010)

wkdays1 <- filter(final.df, !(wkdays_2010 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2010)

wkdays2 <- filter(final.df, !(wkdays_2011 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2011)

wkdays3 <- filter(final.df, !(wkdays_2012 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2012)

wkdays4 <- filter(final.df, !(wkdays_2013 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2013)

wkdays5 <- filter(final.df, !(wkdays_2014 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2014)

wkdays6 <- filter(final.df, !(wkdays_2015 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2015)

wkdays7 <- filter(final.df, !(wkdays_2016 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2016)




wkday_num_cases_by_yr <- data.frame(num_cases=unname(unlist(c(wkdays1, wkdays2, 
                                                wkdays3, wkdays4,
                                                wkdays5, wkdays6, 
                                                wkdays7))), 
                                    year=as.factor(c(rep(2010, nrow(wkdays1)), 
                                                     rep(2011, nrow(wkdays2)),
                                                     rep(2012, nrow(wkdays3)),
                                                     rep(2013, nrow(wkdays4)),
                                                     rep(2014, nrow(wkdays5)),
                                                     rep(2015, nrow(wkdays6)),
                                                     rep(2016, nrow(wkdays7))
                                                     )))
str(wkday_num_cases_by_yr)

# FIT MODEL FOR WEEKDAYS --------------------------------
plot(num_cases~year, data=wkday_num_cases_by_yr, 
     main="Distribution of number of daily visits in Oct-Nov, \nby year (Wkdays only)", 
     ylab="num visits", xlab="Year")

m2 <- lm(num_cases~year, data=wkday_num_cases_by_yr)
anova(m2)

summary(m2)  # F-Stat significant; all coeffs significant
sqrt(mean(resid(m2)^2))  # RMSE = 12.24768 visits 

# > model diagnostics: ------------ 
reset.par <- par(mfrow=c(2,2))  # reset.par is later passed to par to... reset par! 

plot(m2)  # looks good!
plot(resid(m2))  # looks good 
plot(density(resid(m2)))  # looks good 
qqnorm(resid(m2))  # looks good

par(reset.par)


confint(m2)
predict(m2, interval="predict")  # for 2016: [fit,lwr, upr] = [161.6744 137.0069 186.3420]


# Plotting model fit: ---------------------------------------
plot(c(wkdays7$year_2016, NA,NA), type="l", 
     xlim=c(0,45), 
     breaks=seq(0, 45, 5))
abline(h=161.674, col="forestgreen")  # see excel file for source of this number 
abline(h=156.42, lty=5, col="forestgreen")
abline(h=166.93, lty=5, col="forestgreen")
abline(h=137.01, lty=3, col="forestgreen")
abline(h=186.34, lty=3, col="forestgreen")

abline(h=166.701, col="orange")
abline(h=173.511, col="red")



# FIT MODEL FOR WEEKENDS: ------------------------------------------------
wkends1 <- filter(final.df, (wkdays_2010 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2010)

wkends2 <- filter(final.df, (wkdays_2011 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2011)

wkends3 <- filter(final.df, (wkdays_2012 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2012)

wkends4 <- filter(final.df, (wkdays_2013 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2013)

wkends5 <- filter(final.df, (wkdays_2014 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2014)

wkends6 <- filter(final.df, (wkdays_2015 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2015)

wkends7 <- filter(final.df, (wkdays_2016 %in% c("Saturday", "Sunday"))) %>% 
  select(year_2016)


wkend_num_cases_by_yr <- data.frame(num_cases=unname(unlist(c(wkends1, wkends2, 
                                                              wkends3, wkends4,
                                                              wkends5, wkends6, 
                                                              wkends7))), 
                                    year=as.factor(c(rep(2010, nrow(wkends1)), 
                                                     rep(2011, nrow(wkends2)),
                                                     rep(2012, nrow(wkends3)),
                                                     rep(2013, nrow(wkends4)),
                                                     rep(2014, nrow(wkends5)),
                                                     rep(2015, nrow(wkends6)),
                                                     rep(2016, nrow(wkends7))
                                    )))

str(wkend_num_cases_by_yr)

# PLOT MODEL FIT FOR WEEKENDS --------------------------------
plot(num_cases~year, data=wkend_num_cases_by_yr, 
     main="Distribution of number of daily visits in Oct-Nov, \nby year (Weekends only)", 
     ylab="num visits", xlab="Year")

m3 <- lm(num_cases~year, data=wkend_num_cases_by_yr)
anova(m3)  # F-stat highly significant! 

summary(m3); # F-stat highly significant; only 2011 coeff insignificant 
sqrt(mean(resid(m3)^2))  # RMSE = 12.21023 visits 

# > model diagnostics: ------------ 
reset.par <- par(mfrow=c(2,2)) 

plot(m3)
plot(resid(m3))  # increasing variance?  
plot(density(resid(m3)))  # not ideal 
qqnorm(resid(m3))  # looks okay 

par(reset.par)

confint(m3)
predict(m3, interval="predict")  # for 2016: [fit,lwr, upr] = [161.6744 137.0069 186.3420]


# output results of models: --------------
weekday.model <- summary(m2)$coefficients %>% as.data.frame 
weekend.model <- summary(m3)$coefficients %>% as.data.frame 

write.csv(weekday.model, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.04.12 LGH ED Projections for Oct-Nov 2017/output from src/weekday.model.csv")

write.csv(weekend.model, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.04.12 LGH ED Projections for Oct-Nov 2017/output from src/weekend.model.csv")


