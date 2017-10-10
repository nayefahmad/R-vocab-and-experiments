library("RODBC", lib="H:/R packages")
library("ggplot2", lib="H:/R packages")
library("labeling", lib="H:/R packages")
library("digest", lib="H:/R packages")
library("reshape2", lib="H:/R packages")
library("magrittr", lib="H:/R packages")
require("reshape2", lib="H:/R packages")
require("dplyr", lib="H:/R packages")
require("lazyeval", lib="H:/R packages")

# rm(list=ls())

data <- data.frame(unit=c("6E", "6E", "6W", "6W"), 
                   year=as.factor(c(2016, 2017, 2016, 2017)), 
                   census=c(30, 22, 80, 121))
data

data.m <- melt(data, id.vars=c("unit", "year"))
data.m

# p1: slope graph 
p1 <- ggplot(data=data.m) + 
            geom_line(aes(x=year, y=value, group=unit, colour=unit)) 
p1 

# p2: doesn't work without "group" in aes()----------------
p2 <- ggplot(data=data.m) + 
            geom_line(aes(x=year, y=value, colour=unit))
p2
