
#*******************************************
# GENERATING BLOCK OF TREATMENTS TO BE REPEATED OVER A SINGLE DAY 
#*******************************************

# rm(list = ls())

source("2017-12-05_vgh_mdc-treatment-duration-ecdfs.R")

# TODO: ---------------
# expand block to randomly choose lower prob treatments 



#*********************************************
# get column sums: 
df1.colsums <- apply(df1.durations[ ,2:ncol(df1.durations)]
                     , 2, sum)

# let's focus on the top 5 for now: 
df1.colsums <- df1.colsums[order(df1.colsums)] %>% rev
df1.colsums <- df1.colsums[1:5]

df1.colsums.rescaled <- sapply(df1.colsums, 
                               function(x){x / min(df1.colsums) %>% ceiling}) # %>% print 


# create a df: 
block <- data.frame(treatment = names(df1.colsums.rescaled), 
                    num.cases.per.block = df1.colsums.rescaled)
rownames(block) <- NULL

