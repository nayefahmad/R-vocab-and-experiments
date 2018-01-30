
#*******************************
# TESTING JOINS: 
#*******************************

library("magrittr")
library("dplyr")

df1 <- data.frame(lha=c("ubc",
                        "ubc",
                        "downt",
                        "downt", 
                        "uptwn", 
                        "uptwn"), 
                  gender= rep(c("m", "f"), times=6), 
                  ctas=sample(5, 12, replace = TRUE))  %>% print 

df2 <- data.frame(lha=c("ubc", "downt", "uptwn"), 
                  population=rpois(3, 20)) %>% print 


# inner join: 
inner_join(df1, df2)

