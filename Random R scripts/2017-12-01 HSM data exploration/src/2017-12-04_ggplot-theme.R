
#*****************************
# define theme for plots 
#*****************************

mytheme <- theme(axis.text.x=element_text(size = 14),
                 axis.text.y=element_text(size=14),
                 
                 axis.title.x=element_text(size=20), 
                 axis.title.y=element_text(size=18), 
                 
                 strip.text.x=element_text(size=18), 
                 
                 panel.background = element_rect(fill = NA, 
                                                 colour="black"), 
                 plot.title = element_text(size=24), 
                 plot.subtitle = element_text(size=20), 
                 
                 legend.text = element_text(size=14)
                 
                 )
