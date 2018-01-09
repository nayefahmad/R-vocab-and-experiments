
#**********************************************
# RANDOM GGPLOT STUFF 
#**********************************************


library("ggplot2")
library("labeling")
# library("digest")
library("reshape2")
library("magrittr")

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





# USING GGSAVE: --------------
# first create a plot object: 
p1 <- mtcars %>% 
      ggplot(aes(x=disp, y=drat)) + 
      geom_point(colour="dodgerblue") + 
      labs(title = "mtcars: drat vs disp") + 
      theme_classic(base_size = 16); p1 

# now use ggsave: 
ggsave(file = "H:/VCH files - Nayef/R-vocab-and-experiments/ggsave-test.pdf", 
       plot = p1  # default is to use last plot, so you might be able to omit 
                  # this arg 
       )
