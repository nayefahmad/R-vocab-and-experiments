

#**********************************************
# IMPORTING AND CLEANING HSM DATA 
#**********************************************

library("tidyr")
library("ggplot2")
library("openxlsx")
library("magrittr")
library("dplyr")
library("ff")
library("data.table")
library("here")

# rm(list = ls())

options("fftempdir"= here("results", "output from src"))
# options("ffdrop"= FALSE)  # try to stop deletion of ff files on restart R
                            # doesn't work 

# TODO: ---------------
# > retain ffdf files so that they can be used later: don't overwrite with dfs
# > fn for data wrangling 

# NOTES: ----------------
# NULL 



#***************************************
# 1. READ IN DATA --------
#***************************************

# > READ IN ALL FYEAR 14-15 DATA:----------
# first use read.csv( ) to examine and recode cols as the right type of data
df1.1.hsm.fyear14_15 <- 
      read.csv(file=here("data", "2017-12-05_hsm_fyear14-15-data.csv"), 
               header=FALSE, nrow =  5)

col.class <- sapply(df1.1.hsm.fyear14_15, class)
col.class[grep("integer", col.class)] <- "factor"


# now use read.csv.ff( ): 
df1.1.hsm.fyear14_15 <- 
      read.csv.ffdf(file=here("data", "2017-12-05_hsm_fyear14-15-data.csv"), 
               header=FALSE, 
               colClasses = col.class)


#***************************************
# > READ IN ALL FYEAR 09-10 DATA:----------
# first use read.csv( ) to examine and recode cols as the right type of data
df1.2.hsm.fyear09_10 <- 
      read.csv(file=here("data", "2017-12-11_hsm_fyear09-10-data.csv"), 
               header=FALSE, nrow =  5)

col.class <- sapply(df1.2.hsm.fyear09_10, class)
col.class[grep("integer", col.class)] <- "factor"


# now use read.csv.ff( ): 
df1.2.hsm.fyear09_10 <- 
      read.csv.ffdf(file=here("data", "2017-12-11_hsm_fyear09-10-data.csv"), 
                    header=FALSE, 
                    colClasses = col.class)



# > read in FYEAR 09-10 DATA WITH AGE: ----------
df1.2.1.hsm.fyear09_10 <- 
      read.csv(file=here("data", "2018-02-02_hsm_fyear09-10-data-with-age-hsda.csv"), 
               header=TRUE) 




#***************************************
# > READ IN ALL FYEAR 10-11 DATA:----------
# first use read.csv( ) to examine and recode cols as the right type of data
df1.3.hsm.fyear10_11 <- 
      read.csv(file=paste0(path, "/data/2017-12-11_hsm_fyear10-11-data.csv"), 
               header=FALSE, nrow =  5)

col.class <- sapply(df1.3.hsm.fyear10_11, class)
col.class[grep("integer", col.class)] <- "factor"
col.class[grep("logical", col.class)] <- "factor"


# now use read.csv.ff( ): 
df1.3.hsm.fyear10_11 <- 
      read.csv.ffdf(file=paste0(path, "/data/2017-12-11_hsm_fyear10-11-data.csv"), 
                    header=FALSE, 
                    colClasses = col.class)





#***************************************
# 2. DATA WRANGLING -------------
#***************************************
# > DATA WRANGLING FOR ALL FY14-15 DATA: -----------------
df2.1.cols <- c(3, 4, 5, 8, 10, 11)  # columns we want to select

# subset cols using data.table: 
df2.1.hsm.fyear14_15 <- df1.1.hsm.fyear14_15[ ,df2.1.cols]

names(df2.1.hsm.fyear14_15) <- c("fyear", 
                                 "hsm.id", 
                                 "pt.id",  
                                 "hsda", 
                                 "p.seg", 
                                 "p.seg.last.year")

# add cols with pseg codes (dplyr seems not to work with ff) : 
df2.1.hsm.fyear14_15$p.seg.code.14_15 <- substr(df2.1.hsm.fyear14_15$p.seg, 1, 4) %>% as.factor

df2.1.hsm.fyear14_15$p.seg.last.year.code <- substr(df2.1.hsm.fyear14_15$p.seg.last.year, 1, 4) %>% as.factor

# str(df2.1.hsm.fyear14_15)  # ; summary(df2.1.hsm.fyear14_15)


#**************************************************************
# > DATA WRANGLING FOR ALL FY09-10 DATA: -----------------
df2.2.cols <- c(3, 4, 5, 8, 10, 11)  # columns we want to select

# subset cols using data.table: 
df2.2.hsm.fyear09_10 <- df1.2.hsm.fyear09_10[ ,df2.2.cols]

names(df2.2.hsm.fyear09_10) <- c("fyear", 
                                 "hsm.id", 
                                 "pt.id",
                                 "hsda", 
                                 "p.seg", 
                                 "p.seg.last.year")

df2.2.hsm.fyear09_10$p.seg.code.09_10 <- substr(df2.2.hsm.fyear09_10$p.seg, 1, 4) %>% as.factor

df2.2.hsm.fyear09_10$p.seg.last.year.code <- substr(df2.2.hsm.fyear09_10$p.seg.last.year, 1, 4) %>% as.factor

str(df2.2.hsm.fyear09_10)  #; summary(df2.2.hsm.fyear09_10)


# > DATA WRANGLING FOR fy09-10 DATA WITH AGE: -------
names(df1.2.1.hsm.fyear09_10) <- c("fyear", 
                                   "hsm.id", 
                                   "pt.id",
                                   "age", 
                                   "hsda", 
                                   "p.seg", 
                                   "p.seg.last.year")

df1.2.1.hsm.fyear09_10$p.seg.code.09_10 <- substr(df1.2.1.hsm.fyear09_10$p.seg, 1, 4) %>% as.factor

df1.2.1.hsm.fyear09_10$p.seg.last.year.code <- substr(df1.2.1.hsm.fyear09_10$p.seg.last.year, 1, 4) %>% as.factor

df1.2.1.hsm.fyear09_10$age <- df1.2.1.hsm.fyear09_10$age %>% as.numeric


# str(df1.2.1.hsm.fyear09_10) 
# summary(df1.2.1.hsm.fyear09_10)
# summary(df1.2.1.hsm.fyear09_10$p.seg.code.09_10)





#**************************************************************
# > DATA WRANGLING FOR ALL FY10-11 DATA: -----------------
df2.3.cols <- c(3, 4, 5, 10, 11)  # columns we want to select

# subset cols using data.table: 
df2.3.hsm.fyear10_11 <- df1.3.hsm.fyear10_11[ ,df1.3.cols]

names(df2.3.hsm.fyear10_11) <- c("fyear", 
                                 "hsm.id", 
                                 "pt.id",  
                                 "p.seg", 
                                 "p.seg.last.year")

df2.3.hsm.fyear10_11$p.seg.code.10_11 <- substr(df2.3.hsm.fyear10_11$p.seg, 1, 4) %>% as.factor

df2.3.hsm.fyear10_11$p.seg.last.year.code <- substr(df2.3.hsm.fyear10_11$p.seg.last.year, 1, 4) %>% as.factor

str(df2.3.hsm.fyear10_11)  #; summary(df2.2.hsm.fyear09_10)







#**************************************************************
# 3. JOINING TABLES --------
#**************************************************************

# > JOINING 09-10 DATA WITH 14-15 DATA --------

# using package data.table:
t1.1.hsm.fyear14_15 <- as.data.table(select(df1.1.hsm.fyear14_15, c(2, 6)))

# filter starting population if necessary: todo: 
t1.2.hsm.fyear09_10 <- as.data.table(select(df1.2.hsm.fyear09_10, c(2, 6)))

# set columns to join on: 
setkey(t1.1.hsm.fyear14_15, hsm.id)
setkey(t1.2.hsm.fyear09_10, hsm.id)

# join and reorder cols: 
join.t1.1_t1.2 <- t1.1.hsm.fyear14_15[t1.2.hsm.fyear09_10]  
# this is data.table join syntax for LEFT JOIN, with t1.2.hsm.fyear09_10 on LEFT

join.t1.1_t1.2 %<>% select(hsm.id, p.seg.code.09_10, p.seg.code.14_15)


str(join.t1.1_t1.2); summary(join.t1.1_t1.2)



#************************************************************************
# > JOINING filtered O9-10 DATA with 14-15 data, using age and COC: --------
#************************************************************************
# filter 09-10 data for age: 
df1.2.2.hsm.fyear09_10 <- 
      filter(df1.2.1.hsm.fyear09_10, 
                                 age %in% 65:200) %>% 
      select(hsm.id, 
             age, 
             p.seg.code.09_10) 

# str(df1.2.2.hsm.fyear09_10)
# summary(df1.2.2.hsm.fyear09_10)


# filter 14-15 data for COC: 
df2.1.1.fyear14_15.filter <- 
      filter(df2.1.hsm.fyear14_15,
             hsda == "33 North Shore/Coast Garibaldi") %>%
      select(# df2.1.hsm.fyear14_15, 
             hsm.id, 
             hsda, 
             p.seg.code.14_15)

# str(df2.1.1.fyear14_15.filter)
# summary(df2.1.1.fyear14_15.filter)
# summary(df2.1.1.fyear14_15.filter$hsda)

#************************
# >> NOW JOIN FILTERED DFs: ---------------

# using package data.table:
t2.1.hsm.fyear14_15.filter <- as.data.table(df2.1.1.fyear14_15.filter)
t2.2.hsm.fyear09_10.filter <- as.data.table(df1.2.2.hsm.fyear09_10)

# set columns to join on: 
setkey(t2.1.hsm.fyear14_15.filter, hsm.id)
setkey(t2.2.hsm.fyear09_10.filter, hsm.id)

# join and reorder cols: 
join.t2.1_t2.2.filter <- t2.1.hsm.fyear14_15.filter[t2.2.hsm.fyear09_10.filter]  # data.table join syntax
join.t2.1_t2.2.filter %<>% select(hsm.id, age, hsda, p.seg.code.09_10, p.seg.code.14_15)


# str(join.t2.1_t2.2.filter)
# summary(join.t2.1_t2.2.filter)
# summary(join.t2.1_t2.2.filter$hsda)  # todo: why so many NAs? 
# summary(join.t2.1_t2.2.filter$p.seg.code.14_15)  # todo: why so many NAs? 
# head(join.t2.1_t2.2.filter)







#************************************************
# > JOINING 09-10 DATA WITH 10-11 DATA --------

# using package data.table: 
t1.3.hsm.fyear10_11 <- as.data.table(select(df1.3.hsm.fyear10_11, c(2, 6)))

# filter starting population if necessary: todo: 
t1.2.hsm.fyear09_10 <- as.data.table(select(df1.2.hsm.fyear09_10, c(2, 6)))


# set columns to join on: 
setkey(t1.3.hsm.fyear10_11, hsm.id)
setkey(t1.2.hsm.fyear09_10, hsm.id)

# join and reorder cols: 
join.t1.3_t1.2 <- t1.3.hsm.fyear10_11[t1.2.hsm.fyear09_10]  # data.table join syntax
join.t1.3_t1.2 %<>% select(hsm.id, p.seg.code.09_10, p.seg.code.10_11)


str(join.t1.3_t1.2); summary(join.t1.3_t1.2)



#**********************************************
# Write the data to csv: ------------
#**********************************************
output.path <- "G:/Projects (Dept VC)/Patient Flow Project/VC/Health System Matrix/2017-12-01 HSM data exploration/results/output from src"


write.csv(df1.hsm.data, file = "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/VC/Health System Matrix/2017-12-01 HSM data exploration/results/output from src/2017-12-01_hsm_ps13-reformatted-data.csv", 
          row.names = FALSE)


write.csv(df2.1.hsm.fyear14_15, file = "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/VC/Health System Matrix/2017-12-01 HSM data exploration/results/output from src/2018-02-05_hsm_fyear-14_15-reformatted-data.csv", 
          row.names = FALSE)

write.csv(df2.2.hsm.fyear09_10, file = "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/VC/Health System Matrix/2017-12-01 HSM data exploration/results/output from src/2018-02-05_hsm_fyear-09_10-reformatted-data.csv", 
          row.names = FALSE)


# save outputs for filtered datasets: todo: 
write.csv(df1.2.1.hsm.fyear09_10, 
          here("output from src", "2018-02-01_hsm_fyear-09_10-reformatted-data-with-age.csv"), 
          row.names = FALSE)




#*****************************************************
# p.seg breakdowns for fy09-10, fy10-11
write.csv(df1.2.hsm.fyear09_10 %>% 
                group_by(p.seg.code.09_10) %>% 
                summarise(count = n()), 
          file = paste0(output.path, "/2017-12-12_hsm_population-breakdown-fy09_10.csv"), 
          row.names = FALSE)

write.csv(df1.1.hsm.fyear14_15 %>% 
                group_by(p.seg.code.14_15) %>% 
                summarise(count = n()), 
          file = paste0(output.path, "/2017-12-12_hsm_population-breakdown-fy14_15.csv"), 
          row.names = FALSE)


write.csv(df1.3.hsm.fyear10_11 %>% 
                group_by(p.seg.code.10_11) %>% 
                summarise(count = n()), 
          file = paste0(output.path, "/2017-12-13_hsm_population-breakdown-fy10_11.csv"), 
          row.names = FALSE)


# save outputs for filtered datasets: --------------------- 
# save 14-15 filtered by COC: 
write.csv(df2.1.1.fyear14_15.filter %>% 
                group_by(p.seg.code.14_15, hsda) %>% 
                summarise(count = n()), 
          file = paste0(output.path, "/2018-02-02_hsm_coastal-population-breakdown-fy14_15.csv"), 
          row.names = FALSE)


# save 09-10 filtered by age: 
write.csv(df1.2.2.hsm.fyear09_10 %>% 
                group_by(p.seg.code.09_10) %>% 
                summarise(count = n(), 
                          age.ceiling = max(age)), 
          file = paste0(output.path, "/2018-02-02_hsm_age64-population-breakdown-fy09_10.csv"), 
          row.names = FALSE)


# save 09-10 filtered by age and hsda(from joining with 14-15): 
write.csv(join.t2.1_t2.2.filter %>% 
                na.omit %>% 
                group_by(p.seg.code.09_10) %>% 
                summarise(count = n(), 
                          age.ceiling = max(age)), 
          file = paste0(output.path, "/2018-02-02_hsm_age200-hsda_coast_population-breakdown-fy09_10.csv"), 
          row.names = FALSE)


#*****************************************************
# save fy14-15 data as RDS: -----------
saveRDS(df1.1.hsm.fyear14_15, 
        file = paste0(output.path, "/2018-01-18_hsm_fyear-14-15-data.rds"))
