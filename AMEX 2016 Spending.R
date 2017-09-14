library("ggplot2")
library("plyr")

# rm(list=ls())
amex <- read.csv(file.choose(), stringsAsFactors = FALSE)

attach(amex)

str(amex)

amex2 <- data.frame(Date = as.Date(Date), 
                Merchant = as.factor(Merchant), 
                Amount = as.double(gsub(",", "", Amount)), 
                Country = as.factor(Country), 
                Category = as.factor(Category),
                Subcategory = as.factor(Subcategory))

str(amex2)
amex2

detach(amex)

attach(amex2)

# ggplot(amex2, aes(x=Amount)) + 
#     geom_histogram(binwidth = 1000)
# This is being thrown off by some huge values

range(Amount)  # -1510.13 17274.68

# ggplot(subset(amex2, amex2$Amount < 17000 & amex2$Amount >=0), aes(x=Amount)) + 
#     geom_histogram(binwidth = 500)

# ggplot(subset(amex2, (amex2$Amount >=0)), aes(x=Amount)) + 
#     geom_histogram(breaks=c(seq(0,2000, by=100), max(Amount)), binwidth = 100)
# this is not working 
# how to add a "more" bar on the histogram? 

detach(amex2)

amex2.large.values <- amex2

subset(amex2.large.values, (amex2.large.values$Amount >= 0 & amex2.large.values$Amount > 2500))  # only 2 values above $2500

amex2.large.values$Amount[amex2.large.values$Amount > 2500] <- 2500
amex2.large.values.2 <- subset(amex2.large.values, amex2.large.values$Amount > 0)

amex2.large.values.2$Amount

ggplot(amex2.large.values.2, aes(x=amex2.large.values.2$Amount)) + 
    geom_histogram(breaks=c(seq(0,2500, by=100))) + 
    xlab("Amount") + ylab("Frequency") + 
    ggtitle("Histogram of amount spent") + 
    scale_x_continuous(limits=c(0, 2500), breaks=c(seq(0,2500, by=500)), labels = c(seq(0, 2000, by=500), "2500+")) + 
    theme_minimal() + 
    theme(axis.text.x=element_text(size = 12),  
              plot.title=element_text(size=24))
