
#************************************************************
# FINDING AN "INCIDENCE MATRIX" FROM RATINGS BY 2 INDIVIDUALS
#************************************************************

library("dplyr")
library("magrittr")
library("reshape2")


# define function: -------------------------------------------------
incidence.matrix <- function(dataframe, scale.max){
      
      # inputs: 
      # > dataframe: raw data, 1 row for each question, 1 col for each reviewer 
      #      recording their scoring for that question
      # > scale.max: max rating possible (7? 10?)
      
      # record old name of cols: 
      old.col.2 <- colnames(dataframe)[2]
      old.col.3 <- colnames(dataframe)[3]
      
      # for convenience, rename cols: 
      colnames(dataframe)[2] <- "alice"
      colnames(dataframe)[3] <- "bob"
      
      # print(dataframe)
      
      # all possible combos: 
      combos <- data.frame(alice = rep(1:scale.max, each=scale.max), 
                           bob = rep(1:scale.max, times=scale.max))
      
      # tabulate and dcast: 
      matrix <- dataframe %>% 
            group_by(alice, bob) %>% 
            summarise(num = n()) %>% 
            full_join(combos, 
                      by = c("alice" = "alice", 
                             "bob" = "bob")) %>% 
            arrange(alice, bob) %>% 
            dcast(alice ~ bob) 
      
      # remove NAs: 
      matrix[is.na(matrix)] <- 0 
      names(matrix)[1] <- paste0(old.col.2, "/", old.col.3)
      
      return(matrix)
      
}


# test function: -------------------------------------------------

data <- data.frame(question = seq(20), 
                   rater1 = sample(1:7, 20, replace = TRUE), 
                   rater2 = sample(1:7, 20, replace = TRUE)) %>% print

incidence.matrix(data, 7)




# other checks: 
# filter(data,
#        rater1==2)
# 
# filter(data,
#        rater2==3)
