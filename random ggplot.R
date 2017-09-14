library("RODBC", lib="H:/R packages")
library("ggplot2", lib="H:/R packages")
library("labeling", lib="H:/R packages")
library("digest", lib="H:/R packages")
library("reshape2", lib="H:/R packages")
library("magrittr", lib="H:/R packages")

# rm(list=ls())

str(mpg)

mpg2 <- mpg
mpg2$manufacturer <- factor(mpg2$manufacturer)

str(mpg2)


# overlaid bar graphs: 
ggplot(NULL, aes(x=manufacturer)) + 
  geom_bar(data=mpg2, stat='identity', aes(y=hwy), alpha=.9, fill="dodgerblue") + 
  geom_bar(data=mpg2, stat="identity", aes(y=cty), alpha=.25, fill="firebrick") + 
  labs(x="Manufacturer", y="Hwy & City Mileage") + 
  scale_y_continuous(limits = c(0,1000), breaks=seq(0,1000,100), labels=seq(0,1000,100)) + 
  guides(fill=FALSE, alpha=FALSE) + 
  geom_hline(yintercept=0, size=1) + 
  theme(axis.text.x=element_text(size = 12, angle=45),
        axis.title.x=element_text(size=20),  
        axis.text.y=element_text(size=14), 
        axis.title.y=element_text(size=18),
#         axis.ticks.margin=unit(c(4,-4),'cm'), 
        panel.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = "gray90"),
        panel.border = element_rect(linetype = "dashed", colour = "black", fill=NA)
  )
