
# ***************************************************
# 2017-09-15_LGH_LGH-comparison-with-Fraser-using-CMGs
# SURGICAL UNITS 
# ***************************************************

library("ca")  # help(package="ca")
library("reshape2")
library("dplyr")
library("zoom")
library("ggplot2")
library("ggrepel")

# getwd()
# rm(list=ls())

#************************************************
# todo: -------
# get full list of fhu cmgs 
# improve ggplot graphs 
#************************************************


# read in data: ----------
setwd("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/results/Clean data")

surg_units.orig <- read.csv(
      "2017-09-15_Surgical-units-LGH-and-Fraser.csv",
      stringsAsFactors = TRUE)

surg_units <- surg_units.orig

names(surg_units) <- c("unit", 
                      "cmg", 
                      "count")

# str(surg_units)
# summary(surg_units)

# pivot data: 
surg_units.m <- melt(surg_units)
surg_units.wide <- dcast(surg_units, cmg~unit)

surg_units.wide.copy <- surg_units.wide  # create copy for 
# writing to csv

# replace NAs with zeros: 
surg_units.wide[is.na(surg_units.wide)] <- 0

# create lookup table for rowname: 
cmg_lookup <- data.frame(cmg=surg_units.wide[[1]], 
                         cmg_code = paste("cmg", 
                                          1:nrow(surg_units.wide)))

# drop cmg column from dataset: 
row.names(surg_units.wide) <- cmg_lookup$cmg_code

surg_units.wide <- select(surg_units.wide, -cmg)


# str(surg_units.wide)
# summary(surg_units.wide)
# head(surg_units.wide)

# write final table to csv: 
write.csv(surg_units.wide.copy, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/CMGs-vs-units-crosstab_surgery.csv", 
          row.names=TRUE) 

write.csv(cmg_lookup, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/CMGs-code-lookup_surg.csv", 
          row.names=FALSE)


#***********************************************
# Correspondence analysis: ---------------
#***********************************************
ca_model <- ca(surg_units.wide)
# p1 <- plot(ca_model); p1  # symmetric map
# p2 <- plot(ca_model, map="colprincipal"); p2
# p3 <- plot(ca_model, map="rowprincipal"); p3 

# Using ggplot: --------------------------
# assymetric colprincipal map: 

# extract coordinates from model: 
cmg.coords <- as.data.frame(cacoord(ca_model,
                                    type="standard")[1])
unit.coords <- as.data.frame(cacoord(ca_model,
                                     type="principal")[2])

cmg.coords <- mutate(cmg.coords, 
                     cmg=rownames(cmg.coords))
unit.coords <- mutate(unit.coords, 
                      unit=rownames(unit.coords)) 

colnames(cmg.coords)[1:2] <- c("Dim1", 
                               "Dim2")
colnames(unit.coords)[1:2] <- c("Dim1", 
                                "Dim2")

# plotting: 
p6_ca <- ggplot() +
      geom_point(data=cmg.coords, 
                 aes(x=Dim1, y=Dim2), 
                 shape=4) + 
      
      geom_point(data=unit.coords, 
                 aes(x=Dim1, y=Dim2), 
                 shape=24,
                 col="red") + 
      
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) + 
      
      # add unit labels: 
      geom_text_repel(data=unit.coords,
                      aes(x=Dim1, y=Dim2,
                          label=unit), 
                      col="red", 
                      size=6) + 
      
      # add cmg labels: 
      geom_text_repel(data=cmg.coords,
                      aes(x=Dim1, y=Dim2,
                          label=cmg), 
                      alpha=.2, 
                      size=5) + 
      
      theme(panel.background = element_rect(fill = 'white', 
                                            colour = 'white'), 
            panel.border = element_rect(fill=NA, 
                                        colour='grey25')); p6_ca

pdf(file="//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Correspondence-analysis_Surgical-Units_CMGs-vs-Units.pdf", 
    height= 8.5, 
    width = 14)
p6_ca
dev.off()



# Analysis: --------------
# > distance matrix: ----------
source("//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/src/2017-09-15_LGH_LGH-comparison-with-Fraser-using-CMGs/chi2dist-function.R")

surg.distances <- chi2dist(t(surg_units.wide)) %>% as.matrix
# these are chi2 distances in n dimensions, not necessarily 
# the same as what we see in 2 dimensions? 

write.csv(surg.distances, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Surg-units-distance-matrix_full-dimensionality.csv", 
          row.names=TRUE) 


unit.coords.2d <- unit.coords[,c(1,2)]
rownames(unit.coords.2d) <- unit.coords$unit
surg.distances.2d <- dist(unit.coords.2d) %>% as.matrix 

write.csv(surg.distances.2d, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Surg-units-distance-matrix_2d.csv", 
          row.names=TRUE) 



# > Which CMGs are most closely associated with LGH 6E?---------
cmgs.near.lgh6e <- filter(cmg.coords, 
                          Dim1>0, 
                          Dim2<(-.5))
cmgs.near.lgh6e <- merge(cmg_lookup, cmgs.near.lgh6e, 
                         by.x="cmg_code", 
                         by.y = "cmg") %>% 
      select(1,2)


# Which CMGs are more associated with FHU-S3-surg?  
cmgs.near.FHU_s3_surg <- filter(cmg.coords, 
                            Dim1>0, 
                            Dim2>0) 

cmgs.near.FHU_s3_surg <- 
      merge(cmg_lookup, cmgs.near.FHU_s3_surg, 
            by.x="cmg_code", 
            by.y = "cmg") %>% 
      select(1,2)


# Which CMGs are more associated with LGH-6W? 
cmgs.near.LGH6W <- filter(cmg.coords, 
                           Dim1< (-.5))
cmgs.near.LGH6W <- 
      merge(cmg_lookup, cmgs.near.LGH6W, 
            by.x="cmg_code", 
            by.y = "cmg") %>% 
      select(1,2)


# *********************************************
# write these results to bin: 
write.csv(cmgs.near.lgh6e, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/LGH-6E_closest-CMGS.csv", 
          row.names=TRUE) 

write.csv(cmgs.near.FHU_s3_surg, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/FHU-S3-surg_closest-CMGS.csv", 
          row.names=TRUE) 

write.csv(cmgs.near.LGH6W, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/LGH-6W_closest-CMGS.csv", 
          row.names=TRUE) 


# *********************************************
