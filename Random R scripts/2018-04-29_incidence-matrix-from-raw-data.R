
#************************************************************
# FINDING AN "INCIDENCE MATRIX" FROM RATINGS BY 2 INDIVIDUALS
#************************************************************

library("dplyr")
library("magrittr")
library("reshape2")


data <- data.frame(question = seq(20), 
                   alice = sample(1:10, 20, replace = TRUE), 
                   bob = sample(1:10, 20, replace = TRUE))
data

data %>% group_by(alice, bob) %>% 
      summarise(num = n()) %>% 
      melt(id.vars = c("alice", "bob")) %>% 
      dcast(alice ~ bob)

