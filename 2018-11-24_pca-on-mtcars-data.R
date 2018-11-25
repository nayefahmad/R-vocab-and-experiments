

#************************************************
# PCA WITH MTCARS 
# 2018-11-14
#************************************************

library(tidyverse)
library(broom)
library(ggrepel)
library(here)

# reference: https://www.datacamp.com/community/tutorials/pca-analysis-r 


# set up data --------
# PCA works best with numeric data
df1.mtcars.subset <- 
      mtcars %>% 
      select(-c(vs, am)) 

# PCA with prcomp( ) function: ------
# ?prcomp

mtcars.pca <- prcomp(df1.mtcars.subset, 
                     center = TRUE, 
                     scale = TRUE)

# notes: 
# PCA is sensitive to scales of data, so it makes 
# sense to mean-center and scale data before 
# doing PCA on it. 


# Examining the PCA object: --------
# str(mtcars.pca)  # not useful 
summary(mtcars.pca)
# first PC explains 62% of variance

# components withing the pca "model" object: 
names(mtcars.pca)
mtcars.pca$rotation  # gives the loadings 
mtcars.pca$center  # gives means of variables; check by calling 
                   # summary(df1.mtcars.subset)
mtcars.pca$scale
mtcars.pca$x  # same output as tidy(mtcars.pca), but in wide format

# print and plot: 
print(mtcars.pca)  # todo: are these the factor loadings? 
plot(mtcars.pca)

tidy(mtcars.pca)  # %>% View("tidy(PCA)")
# this seems to give the "coordinates"/"component scores"
# of each car along each PC (without specifying the 
# "factor scores" that specify the linear comb of the 
# orig variables that result in the principal components)



# let's plot the cars on PC1: ----
df2.carnames <- rownames(mtcars) %>% 
      as.data.frame() %>% 
      rename(car = ".")

p1.mtcars.pc1 <- 
      tidy(mtcars.pca) %>% 
      
      # choose PC1 only: 
      filter(PC == 1) %>%
      
      # add car names back: 
      left_join(df2.carnames, 
                by = c("row" = "car")) %>%
      
      # now cbind on full mtcars to get all vars: 
      bind_cols(df1.mtcars.subset) %>% 
      
      mutate(y = rep(1, 32), 
             cyl = as.factor(cyl)) %>%
      
      # plotting: 
      ggplot(aes(x = value, 
                 y = y, 
                 col = cyl, 
                 size = hp, 
                 label = row)) + 
      geom_point() + 
      geom_text_repel(force = 50, 
                      size = 4) + 
      
      theme_light(); p1.mtcars.pc1



# biplots from the PCA result: -----
?biplot
biplot(mtcars.pca)

# using ggplot: 
p2.mtcars.2components <- 
      mtcars.pca$x %>%  
      as.data.frame() %>% 
      bind_cols(df2.carnames) %>% 
      bind_cols(df1.mtcars.subset) %>% 
      mutate(cyl = as.factor(cyl)) %>% 
      
      ggplot(aes(x = PC1, 
                 y = PC2, 
                 label = car, 
                 col = cyl, 
                 size = hp)) + 
      geom_point() + 
      geom_text_repel(force = 5, 
                      size = 4); p2.mtcars.2components




# Project new data onto the new basis: -----
new.car <- data.frame(mpg = 10, 
                      cyl = 10, 
                      disp = 1000, 
                      hp = 400, 
                      drat = 5, 
                      wt = 2, 
                      qsec = 17, 
                      gear = 5, 
                      carb = 8)

# mean-center the new car:  
new.car.centered <- 
      new.car %>% 
      scale(center = mtcars.pca$center)

# rotate to the new basis for the vector space: 
new.car.projected <- 
      new.car.centered %*% 
      mtcars.pca$rotation %>% 
      as.data.frame()
# this gives us the coordinates of new.car along 
# the new basis vectors of the space. 


# plot PC1 with the new car added: 



p3.mtcars.pca.with.newcar <- 
      mtcars.pca$x %>% 
      rbind(new.car.projected) %>% 
      as.data.frame %>% 
      mutate(car = rownames(.)) %>% 
      mutate(car = ifelse(car == "1",
                          "NEWCAR", 
                          car)) %>% 
      
      # now plot it: 
      ggplot(aes(x = PC1, 
                 y = rep(1, 33), 
                 label = car)) +
      geom_point(); p3.mtcars.pca.with.newcar
      
      



#*************************************************
# save outputs --------
#*************************************************
ggsave(here("2018-11-24_mtcars-first-PC.pdf"), 
       p1.mtcars.pc1,
       width = 10)
