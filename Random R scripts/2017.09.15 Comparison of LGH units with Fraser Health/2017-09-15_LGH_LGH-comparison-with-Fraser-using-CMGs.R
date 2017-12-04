
# ***************************************************
# 2017-09-15_LGH_LGH-comparison-with-Fraser-using-CMGs
# ***************************************************

library("ca")  # help(package="ca")
library("reshape2")
library("dplyr")
# library("zoom")
# library("ggplot2")
# library("ggrepel")

# getwd()
# rm(list=ls())

#************************************************
# todo: -------
# get full list of fhu cmgs 
# improve ggplot graphs 
#************************************************


# read in data: ----------
setwd("\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/results/Clean data")

med_units.orig <- read.csv(
      "2017-09-15_Medical-units-LGH-and-Fraser.csv",
      stringsAsFactors = TRUE)

med_units <- med_units.orig

names(med_units) <- c("unit", 
                      "cmg", 
                      "count")

# str(med_units)
# summary(med_units)

# pivot data: 
med_munits.m <- melt(med_units)
med_units.wide <- dcast(med_units, cmg~unit)

med_units.wide.copy <- med_units.wide  # create copy for 
                                    # writing to csv

# replace NAs with zeros: 
med_units.wide[is.na(med_units.wide)] <- 0

# create lookup table for rowname: 
cmg_lookup <- data.frame(cmg=med_units.wide[[1]], 
                         cmg_code = paste("cmg", 
                                          1:nrow(med_units.wide)))

# drop cmg column from dataset: 
row.names(med_units.wide) <- cmg_lookup$cmg_code

med_units.wide <- select(med_units.wide, -cmg)

# 
# str(med_units.wide)
# summary(med_units.wide)
# head(med_units.wide)

# write final table to csv: 
write.csv(med_units.wide.copy, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/CMGs-vs-units-crosstab.csv", 
          row.names=TRUE) 

write.csv(cmg_lookup, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/CMGs-code-lookup.csv", 
          row.names=FALSE) 


#***********************************************
# Correspondence analysis: ---------------
#***********************************************
ca_model <- ca(med_units.wide)
# p1 <- plot(ca_model); p1  # symmetric map
# 
# p2 <- plot(ca_model,
#            map="colprincipal"); p2
# p2.1 <- plot(ca_model,
#              map="rowprincipal"); p2.1
# 
# p3 <- plot(ca_model,
#            map="colprincipal",
#            xlim=c(.5,.5),
#            ylim=c(0,1)); p3
# plot.ca(ca_model)


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
p4_ca <- ggplot() +
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
                                        colour='grey25')); p4_ca


# zoom in on previous plot: 
p5_ca_zoom <- 
      ggplot() + 
      geom_point(data=cmg.coords, 
                 aes(x=Dim1, y=Dim2), 
                 shape=4) + 
      
      geom_point(data=unit.coords, 
                 aes(x=Dim1, y=Dim2), 
                 shape=24,
                 col="red", 
                 size=3) + 
      
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) + 
      
      coord_cartesian(xlim = c(0.1,.6), 
                      ylim = c(0.1,.6)) + 
      
      # add unit labels: 
      geom_text_repel(data=unit.coords,
                      aes(x=Dim1, y=Dim2,
                          label=unit), 
                      col="red") + 
      
      # add cmg labels: 
      geom_text_repel(data=filter(cmg.coords, 
                                  Dim1>0, 
                                  Dim2>0),
                      aes(x=Dim1, y=Dim2,
                          label=cmg), 
                      alpha=.2) + 
      
      theme(panel.background = element_rect(fill = 'white', 
                                            colour = 'white'), 
            panel.border = element_rect(fill=NA, 
                                        colour='grey25')); p5_ca_zoom



# print plots to files: ------
pdf(file="//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Correspondence-analysis_Medical-Units_CMGs-vs-Units.pdf", 
    height= 8.5, 
    width = 14)
p4_ca
p5_ca_zoom
dev.off()


jpeg(file="//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Correspondence-analysis_Medical-Units_CMGs-vs-Units.jpeg", 
     height=600, 
     width=1000)
p4_ca
dev.off()

jpeg(file="//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Correspondence-analysis_Medical-Units_CMGs-vs-Units_zoom.jpeg")
p5_ca_zoom
dev.off()



# **************************************************
# Analysis: --------------
# > distance matrix: ----------
source("//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/src/2017-09-15_LGH_LGH-comparison-with-Fraser-using-CMGs/chi2dist-function.R")

med.distances <- chi2dist(t(med_units.wide)) %>% as.matrix
# these are chi2 distances in n dimensions, not necessarily 
# the same as what we see in 2 dimensions? 

write.csv(med.distances, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Med-units-distance-matrix_full-dimensionality.csv", 
          row.names=TRUE) 


unit.coords.2d <- unit.coords[,c(1,2)]
rownames(unit.coords.2d) <- unit.coords$unit
med.distances.2d <- dist(unit.coords.2d) %>% as.matrix 

write.csv(med.distances.2d, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/Med-units-distance-matrix_2d.csv", 
          row.names=TRUE) 


# > Which CMGs are most closely associated with LGH 4E?---------
cmgs.near.lgh4e <- filter(cmg.coords, 
                          Dim1>.25, 
                          Dim2>.375)
cmgs.near.lgh4e <- merge(cmg_lookup, cmgs.near.lgh4e, 
                         by.x="cmg_code", 
                         by.y = "cmg") %>% 
                   select(1,2)


# Which CMGs are more associated with FHU-N43? 
cmgs.near.FHU_N43 <- filter(cmg.coords, 
                            Dim1< (-1.5)) 

cmgs.near.FHU_N43 <- 
      merge(cmg_lookup, cmgs.near.FHU_N43, 
            by.x="cmg_code", 
            by.y = "cmg") %>% 
      select(1,2)


# Which CMGs are more associated with FHU-T8? 
cmgs.near.FHU_T8 <- filter(cmg.coords, 
                           Dim2< (-1))
cmgs.near.FHU_T8 <- 
      merge(cmg_lookup, cmgs.near.FHU_T8, 
            by.x="cmg_code", 
            by.y = "cmg") %>% 
      select(1,2)



# *********************************************
# write these results to bin: 
write.csv(cmgs.near.lgh4e, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/LGH-4E_closest-CMGS.csv", 
          row.names=TRUE) 

write.csv(cmgs.near.FHU_N43, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/FHU-N43_closest-CMGS.csv", 
          row.names=TRUE) 

write.csv(cmgs.near.FHU_T8, file= "\\\\vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin/FHU-t8_closest-CMGS.csv", 
          row.names=TRUE) 


# *********************************************

