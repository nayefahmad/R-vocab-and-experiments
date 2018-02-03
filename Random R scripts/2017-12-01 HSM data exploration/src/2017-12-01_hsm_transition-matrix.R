

#**********************************************
# HSM: CALCULATING THE TRANSITION MATRIX 
#**********************************************

# rm(list = ls())

# TODO: ---------
# > move graphs to separate script? 
# > need to do an anti-join to find transition from NA to other psegs in 
#     destination year (without NA row, we can't matrix mult)?
#     OR could we just do a full outer join in join.t1_t.2, etc.??? 

#*********************************************
# import data, load packages, themes, and functions: 
# source("2017-12-01_hsm_data-cleaning.R")
source(here("src", "p.seg-sources-distribution_function.R")) 
source(here("src", "2017-12-04_ggplot-theme.R"))
source(here("src", "p.seg-destinations-distribution_function.R"))

# 1. ANALYSIS FOR PS13 ONLY: --------------
# > distributions of SOURCE P.SEGs for all years: -----------
sources.dist(df1.hsm.data, "14-15", "PS13")

# examine all years: 
list1.ps13.sources.all.years <- 
      lapply(levels(df1.hsm.data$fyear),  # list of all years 
             sources.dist, 
             p.segment = "PS13",  # specify other args to sources.dist()
             dataframe = df1.hsm.data)  # ditto  

# > Notes: -------
# > PS04 and PS08 are always missing. This makes sense, of course (children and maternity). 

df2.ps13.sources.all.years <- do.call(rbind, list1.ps13.sources.all.years)

# > graphs to compare distribution across years: -----------
p1.sources.pseg13 <- 
      ggplot(df2.ps13.sources.all.years, 
             aes(x= p.seg.last.year.code, 
                 y=proportions, 
                 group = fyear, 
                 colour=fyear)) + 
      
      scale_y_continuous(expand=c(0,0)) + 
      
      geom_line() + 
      geom_hline(yintercept = 0) + 
      
      labs(title = "PS13: distribution of source population segments", 
           subtitle = "Over past 6 years, there has been very little change in source population segments \nthat transfer into PS13") + 
      
      mytheme; p1.sources.pseg13
      


#*************************************************
# SOURCE DISTRIBUTIONS ANALYSIS FOR FYEAR 14-15: ---------------
sources.dist(df1.1.hsm.fyear14_15, "14/15", "PS13")

list2.fyear1415.all.psegs.sources <- 
      lapply(levels(df1.1.hsm.fyear14_15$p.seg.code), 
             sources.dist, 
             f.year="14/15", 
             dataframe = df1.1.hsm.fyear14_15)


df3.all.psegs.sources <- do.call(rbind, list2.fyear1415.all.psegs.sources)

str(df3.all.psegs.sources)






#*************************************************
# FORWARD TRANSITION MATRICES (DESTINATIONS DISTRIBUTIONS)
#*************************************************

# TRANSITION FROM FYEAR 09_10 TO 14_15: -----
# test with 1 pseg: 
destinations.dist(destination.col = 3, 
                  p.seg = "PS01", 
                  dataframe = join.t1.1_t1.2)

# now run with all psegs: 
list3.fy09_10.to.14_15 <- 
      lapply(levels(join.t1.1_t1.2$p.seg.code.09_10), 
             destinations.dist, 
             destination.col = 3, 
             dataframe = join.t1.1_t1.2)


df4.fy09_10.to.14_15 <- do.call(rbind, list3.fy09_10.to.14_15)

str(df4.fy09_10.to.14_15)

# reshape to matrix format: 
df4.1.matrix <- melt(df4.fy09_10.to.14_15) %>%
      filter(variable == "proportions") %>% 
      dcast(p.seg.code.09_10 ~ p.seg.code.14_15)



#*****************************************************
# > FILTERED TRANSITION MATRIX FROM 09-10 TO 14-15, FILTERED FOR AGE AND COC: ---------
# test with 1 pseg: 
destinations.dist(destination.col = 3, 
                  p.seg = "PS01", 
                  dataframe = select(join.t2.1_t2.2.filter, 
                                     hsm.id, 
                                     p.seg.code.09_10, 
                                     p.seg.code.14_15))


# now run with all psegs: 
list5.fy09_10.to.14_15.filter <- 
      lapply(levels(join.t2.1_t2.2.filter$p.seg.code.09_10), 
             destinations.dist, 
             destination.col = 3, 
             dataframe = select(join.t2.1_t2.2.filter, 
                                hsm.id, 
                                p.seg.code.09_10, 
                                p.seg.code.14_15))


df6.fy09_10.to.14_15.filter <- do.call(rbind, list5.fy09_10.to.14_15.filter) %>% 
      
      # add col identifying hsda age group: 
      mutate(hsda=table(join.t2.1_t2.2.filter$hsda)[table(join.t2.1_t2.2.filter$hsda)>0] %>% 
                   names %>%
                   rep(nrow(df6.fy09_10.to.14_15.filter)) %>%
                   as.factor,

             age.ceiling=rep(max(join.t2.1_t2.2.filter$age), nrow(df6.fy09_10.to.14_15.filter))) 



str(df6.fy09_10.to.14_15.filter)
summary(df6.fy09_10.to.14_15.filter)







#************************************************************
# TRANSITION FROM FYEAR 09_10 TO 10_11: -----
#************************************************************
destinations.dist(destination.col = 3, 
                  p.seg = "PS01", 
                  dataframe = join.t1.3_t1.2)

list4.fy09_10.to.10_11 <- 
      lapply(levels(join.t1.3_t1.2$p.seg.code.09_10), 
             destinations.dist, 
             destination.col = 3, 
             dataframe = join.t1.3_t1.2)


df5.fy09_10.to.10_11 <- do.call(rbind, list4.fy09_10.to.10_11)

str(df5.fy09_10.to.10_11)


# reshape to matrix format: 
df5.1.matrix <- melt(df5.fy09_10.to.10_11) %>%
      filter(variable == "proportions") %>% 
      dcast(p.seg.code.09_10 ~ p.seg.code.10_11)



#**********************************************
# WRITE RESULTS: ---------------------
output.path <- "G:/Projects (Dept VC)/Patient Flow Project/VC/Health System Matrix/2017-12-01 HSM data exploration/results/output from src"

write.csv(df2.ps13.sources.all.years,
          file=paste0(output.path, "/2017-12-04_hsm_ps13-sources-distributions-all-fyears.csv"),
          row.names = FALSE)
                      

write.csv(df3.all.psegs.sources,
          file=paste0(output.path, "2017-12-05_hsm_fyear14_15-sources-distributions-all-pseg.csv"),
          row.names = FALSE)



# ps13 sources all years graph: 
pdf(file = paste0(output.path,
                  "/2017-12-04_hsm_ps13-sources-distributions-all-fyears.pdf"), 
    height = 8.5, width = 14) 
p1.sources.pseg13
dev.off() 

                                                  


# data for 5-year transition matrix from 09-10 to 14-15: 
write.csv(df4.fy09_10.to.14_15,
          file=paste0(output.path, "/2017-12-12_hsm_fyear09_10-transition-matrix-5-years.csv"),
          row.names = FALSE)

write.csv(df4.1.matrix,
          file=paste0(output.path, "/2017-12-12_hsm_fyear09_10-transition-matrix-5-years-wide.csv"),
          row.names = FALSE)


#****************************************************
# data for FILTERED 5-year transition matrix from 09-10 to 14-15: 
write.csv(df6.fy09_10.to.14_15.filter,
          file=paste0(output.path, "/2018-02-02_hsm_filtered-age200-hsda_coast_fyear09_10-transition-matrix-5-years.csv"),
          row.names = FALSE)






# data for 1-year transition matrix from 09-10 to 10-11: 
write.csv(df5.fy09_10.to.10_11,
          file=paste0(output.path, "/2017-12-13_hsm_fyear09_10-transition-matrix-1-year.csv"),
          row.names = FALSE)

write.csv(df5.1.matrix,
          file=paste0(output.path, "/2017-12-13_hsm_fyear09_10-transition-matrix-1-year-wide.csv"),
          row.names = FALSE)
