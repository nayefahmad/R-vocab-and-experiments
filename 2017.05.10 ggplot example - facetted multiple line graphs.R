

library("ggplot2")
library("reshape2")
library("magrittr")
require("reshape2")
require("dplyr")

# rm(list=ls())

# CREATE DATA: ---------------------------
foo <- data.frame(var1 = rnorm(3),
                  var2 = rnorm(3, mean=.8),
                  var3 = rnorm(3, mean=.9),
                  var4 = rnorm(3, mean=1))

qux <- data.frame(var1 = rnorm(3),
                  var2 = rnorm(3, mean=.3),
                  var3 = rnorm(3, mean=.4),
                  var4 = rnorm(1, mean=2))

bar <- data.frame(var1 = rnorm(3,mean=.4),
                  var2 = rnorm(3, mean=.3),
                  var3 = rnorm(3, mean=.9),
                  var4 = rnorm(3, mean=1))

foo <- cbind(ID=as.factor(1:3),foo)
bar <- cbind(ID=as.factor(1:3),bar)
qux <- cbind(ID=as.factor(1:3),qux)

foo$fn <- "foo"
qux$fn <- "qux"
bar$fn <- "bar"

str(foo); str(qux); str(bar)

alldf<-rbind(foo,qux,bar) %>% select(1,6, 2:5)
str(alldf); alldf

# melt data: 
alldf.m <- melt(alldf, id.vars = c("fn", "ID"))  
# Remember, for each combination of "fn" and "ID", there is a single value of each of the other variables (1 value of var1, 1 of var2, etc.). This is why "fn" and "ID" are the id.vars 
# compare: melt(alldf), 45 vs 36 rows 
# also compare: melt(alldf, id.vars = c("ID", "fn"))
str(alldf.m); alldf.m

# CREATE PLOT: -----------------------------------
p1 <- ggplot(data=alldf.m, 
             aes(x=variable, 
                 y = value, 
                 colour = ID, 
                 group = ID)) + 
      
      geom_line() + 
      facet_wrap(~fn); p1 


# VERSION 2: -------------------------------------
# In this version, we plot var1 as a time series (3 separate values), facetted by fn=bar/foo/qux (ignore ID as a separate variable, just use it as an index). Then we plot var3 in the same way. 

str(alldf); alldf
str(alldf.m); alldf.m

# ...Translating to actual use case: ------
# fn = Nursing Unit (ED, ICU, etc.)
# ID = date
# var1 = forecast census 
# var3 = actual census 

# we fill use the following 2 subsets of alldf.m: 
# filter(alldf.m, variable=="var3"); filter(alldf.m, variable=="var1")

# ...p2:---------------
p2 <- ggplot() + 
      # add data and aesthetic in subsequent layers, since they are different
      # for the 2 geom_line layers 
      scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,1),
                         labels=seq(-5,5,1)) + 
      
      # Add actual data: 
      geom_line(data=filter(alldf.m, variable=="var3"),
                aes(x=ID, y=value, group=fn), col="firebrick") + 
            # ID just used as an index for subsequent values of var1 and var3
            # TODO: what exactly is "group=fn" doing? 
                  # Ans. Try removing facet_wrap to see. You will get 3 lines 
                  # of var3 data, grouped by "fn", but in a single panel -> cf.
                  # graph p3
                  # Drop "group=fn" and facetting, and you have a single panel 
                  # that only shows the ranges of var3 for each index, not 
                  # 3 continuous lines -> cf. graph p4
      
      geom_line(data=filter(alldf.m, variable=="var1"), 
                aes(x=ID, y=value, group=fn), col="dodgerblue") +
                     
#       # Add legend: 
#       scale_colour_manual(name="test", 
#                         values=c(var3="red", var1="blue")) + 
      
      facet_wrap(~fn) + 
      
      # Aesthetics: 
      theme_minimal() + 
      labs(x="Index", y= "Value") + 
      geom_hline(yintercept=0, col="grey") + 
      theme(axis.text.x=element_text(size = 14),
            axis.title.x=element_text(size=20), 
            axis.text.y=element_text(size=14), 
            axis.title.y=element_text(size=18), 
            strip.text.x=element_text(size=18),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)); p2 
      



# TODO: how to add legend? 
#...p2.2 Add legend: --------------
p2.2 <- ggplot() + 
      # add data and aesthetic in subsequent layers, since they are different
      # for the 2 geom_line layers 
      scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,1),
                         labels=seq(-5,5,1)) + 
      
      # Add actual data: 
      geom_line(data=filter(alldf.m, variable=="var3"),
                aes(x=ID, y=value, group=fn, col="Actual")) + 
            # ggplot automatically creates legends for colours that are 
                  # passed to aes(). You can later adjust the legend 
                  # with scale_colour_manual. 
      
      geom_line(data=filter(alldf.m, variable=="var1"), 
                aes(x=ID, y=value, group=fn, col="Forecast")) +
      
      # Add legend: 
      scale_colour_manual(name=NULL, 
                          values=c(Actual="firebrick", Forecast="dodgerblue")) + 
      # facetting:  
      facet_wrap(~fn) + 
      
      # Aesthetics: 
      theme_minimal() + 
      labs(x="Index", y= "Value") + 
      geom_hline(yintercept=0, col="grey") + 
      theme(axis.text.x=element_text(size = 14),
            axis.title.x=element_text(size=20), 
            axis.text.y=element_text(size=14), 
            axis.title.y=element_text(size=18), 
            strip.text.x=element_text(size=18),
            panel.border = element_rect(colour = "black", 
                                        fill=NA, size=.5),
            legend.text = element_text(colour="black", size=12, 
                                       face="bold")
            ); p2.2  


# OTHER EXPERIMENTS: --------------------
#...p3 (variant of p2 that's not useful) ----------------------
p3 <- ggplot() + 
      # add data and aesthetic in subsequent layers, since they are different
      # for the 2 geom_line layers 
      scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,1),
                         labels=seq(-5,5,1)) + 
      
      # Add actual data: 
      geom_line(data=filter(alldf.m, variable=="var3"),
                aes(x=ID, y=value, group=fn), col="firebrick") + 
      # ID just used as an index for subsequent values of var1 and var3
      # TODO: what exactly is "group=fn" doing? 
      # Ans. Try removing facet_wrap to see. You will get 3 lines 
      # of var3 data, grouped by "fn", but in a single panel -> cf.
      # graph p3
      # Drop "group=fn" and facetting, and you have a single panel 
      # that only shows the ranges of var3 for each index, not 
      # 3 continuous lines -> cf. graph p4
      
      geom_line(data=filter(alldf.m, variable=="var1"), 
                aes(x=ID, y=value, group=fn), col="dodgerblue") +
      
#       facet_wrap(~fn) + 
      
      # Aesthetics: 
      theme_minimal() + 
      labs(x="Index", y= "Value") + 
      geom_hline(yintercept=0, col="grey") + 
      theme(axis.text.x=element_text(size = 14),
            axis.title.x=element_text(size=20), 
            axis.text.y=element_text(size=14), 
            axis.title.y=element_text(size=18), 
            strip.text.x=element_text(size=18),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) 

# print plot: 
p3

#...p4 (variant of p2 that's not useful) ----------------------
p4 <- ggplot() + 
      # add data and aesthetic in subsequent layers, since they are different
      # for the 2 geom_line layers 
      scale_y_continuous(limits=c(-5, 5), breaks=seq(-5,5,1),
                         labels=seq(-5,5,1)) + 
      
      # Add actual data: 
      geom_line(data=filter(alldf.m, variable=="var3"),
                aes(x=ID, y=value), col="firebrick") + 
      # ID just used as an index for subsequent values of var1 and var3
      # TODO: what exactly is "group=fn" doing? 
      # Ans. Try removing facet_wrap to see. You will get 3 lines 
      # of var3 data, grouped by "fn", but in a single panel -> cf.
      # graph p3
      # Drop "group=fn" and facetting, and you have a single panel 
      # that only shows the ranges of var3 for each index, not 
      # 3 continuous lines -> cf. graph p4
      
      geom_line(data=filter(alldf.m, variable=="var1"), 
                aes(x=ID, y=value), col="dodgerblue") +
      
      #       facet_wrap(~fn) + 
      
      # Aesthetics: 
      theme_minimal() + 
      labs(x="Index", y= "Value") + 
      geom_hline(yintercept=0, col="grey") + 
      theme(axis.text.x=element_text(size = 14),
            axis.title.x=element_text(size=20), 
            axis.text.y=element_text(size=14), 
            axis.title.y=element_text(size=18), 
            strip.text.x=element_text(size=18),
            panel.border = element_rect(colour = "black", fill=NA, size=.5)) 
p4
