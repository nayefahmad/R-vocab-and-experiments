
#************************************************************
# FINDING AN "INCIDENCE MATRIX" FROM RATINGS BY 2 INDIVIDUALS
#************************************************************

library("dplyr")
library("magrittr")
library("reshape2")


data <- data.frame(question = seq(20), 
                   alice = sample(1:10, 20, replace = TRUE), 
                   bob = sample(1:10, 20, replace = TRUE))
print(data)

matrix <- data %>% 
      group_by(alice, bob) %>% 
      summarise(num = n()) %>% 
      dcast(alice ~ bob) %>% as.matrix

matrix[is.na(matrix)] <- 0 

print(matrix)
