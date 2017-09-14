library("ggplot2")
library("plyr")

# rm(list=ls())
potato <- read.csv(file.choose())

attach(potato)
head(potato)
class(Country)  # that's fine 
class(Year)  # should change to date 
class(Area.in.1000ha)  # should change to double 
class(Harvested.production.in.1000t)  # should change to double 
class(Yield..100.kg.ha.)  # should change to double 
class(Main.area..1000.ha.)  # should change to double 

potato2 <- data.frame(Country = potato$Country, 
      Year = potato$Year,
      Area.in.1000ha = as.double(as.character(potato$Area.in.1000ha)),
      Harvested.production.in.1000t = as.double(as.character(potato$Harvested.production.in.1000t)))

# potato2 <- potato2[which(potato2$Country != "European Union (EU6-1972, EU9-1980, EU10-1985, EU12-1994, EU15-2004, EU25-2006, EU27-2013, EU28)")]

potato2 <- subset(potato2, potato2$Country != "European Union (EU6-1972, EU9-1980, EU10-1985, EU12-1994, EU15-2004, EU25-2006, EU27-2013, EU28)" & 
                      potato2$Country != "European Union (28 countries)" &
                      potato2$Year == 2014)

head(potato2)

detach(potato)

# GRAPH 1 -----------------
attach(potato2)

ggplot(potato2, aes(x=Country, y=potato2$Harvested.production.in.1000)) +
    geom_bar(stat="identity") + 
    theme(axis.text.x=element_text(angle=50, size=6, vjust=0.5))

detach(potato2)

# GRAPH 2 -----------------
#let's try ordering the bars: 
ordered.Harvest <- potato2[,c(1, 4)][order(potato2$Harvested.production.in.1000t, potato2$Country), ]

ordered.Harvest$Country <- factor(ordered.Harvest$Country, levels=ordered.Harvest$Country)

# Shorten "Kosovo .... " 
grep("Kosovo", ordered.Harvest$Country)  # row 5?
head(ordered.Harvest)  # yes 
levels(ordered.Harvest$Country)[levels(ordered.Harvest$Country)=="Kosovo (under United Nations Security Council Resolution 1244/99)"] <- "Kosovo"

# shorten "Former Yugoslav Republic of Macedonia, the": 
levels(ordered.Harvest$Country)[levels(ordered.Harvest$Country)=="Former Yugoslav Republic of Macedonia, the"] <- "Macedonia"


ggplot(ordered.Harvest, aes(x=Country, y=ordered.Harvest$Harvested.production.in.1000)) +
    geom_bar(stat="identity") + 
    theme_minimal() + 
    theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) 
    

# TODO: remove countries with NA values for Harvest

ordered.Harvest2 <- subset(ordered.Harvest, is.na(ordered.Harvest$Country) == FALSE & is.na(ordered.Harvest$Harvested.production.in.1000) == FALSE)

str(ordered.Harvest2)

p2 <- ggplot(ordered.Harvest2, aes(x=ordered.Harvest2$Country, y=ordered.Harvest2$Harvested.production.in.1000)) +
    geom_bar(stat="identity") + 
    theme_minimal() + 
    labs(x="Country", y="Harvested prodn in 1000 tons", title="EU Potato Production in 2014") + 
    theme(axis.text.x=element_text(angle=50, size=12, vjust=0.5), 
          axis.title.x=element_text(size=18), 
          axis.title.y=element_text(size = 18), 
          plot.title=element_text(size = 24))

p2 + geom_line(aes(group=1), size=1) +
  geom_point()

