
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
                  ctas=sample(5, 12, replace = TRUE))

df1 %<>% mutate(visits = rpois(nrow(df1), 500)) # %>% print 

df2 <- data.frame(lha=c("ubc", "downt", "uptwn"), 
                  population=rpois(3, 20)) %>% print 


# inner join: 
inner_join(df1, df2) %>% arrange(lha)

# left join: 
left_join(df1, df2) %>% arrange(lha)

# Any difference? No. 
identical(inner_join(df1, df2), left_join(df1, df2))  # TRUE 


# inner join and group by: ---------------------------------------------------
inner_join(df1, df2) %>% 
      group_by(lha, gender, ctas) %>% 
      mutate(util = visits/population,  # correct way to calculate utilization 
             
             # following is the incorrect way: 
             sum.visits = sum(visits), 
             sum.pop = sum(population), 
             util2 = sum.visits/sum.pop, 
             is.util.equal.util2 = ifelse(util == util2, 
                                          TRUE, 
                                          FALSE)) %>% 
      arrange(lha)

# Note: difference between util and util2 is because variable visits 
#     is excluded from the group by statement



# left join a smaller df on a larger one: ----------
df2; df1
left_join(df2, df1)
