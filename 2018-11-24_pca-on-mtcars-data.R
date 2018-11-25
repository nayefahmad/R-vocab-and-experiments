

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
      select(-c(vs, am)) %>% 
      bind_cols(car = rownames(mtcars))


# PCA with prcomp( ) function: ------
?prcomp

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

print(mtcars.pca)  # todo: are these the factor loadings? 

plot(mtcars.pca)

tidy(mtcars.pca)  # %>% View("tidy(PCA)")
# this seems to give the "coordinates"/"component scores"
# of each car along each PC (without specifying the 
# "factor scores" that specify the linear comb of the 
# orig variables that result in the principal components)



# let's plot the cars on PC1: 
p1.mtcars.pc1 <- 
      tidy(mtcars.pca) %>% 
      filter(PC == 1) %>% 
      left_join(df1.mtcars.subset, 
                by = c("row" = "car")) %>% 
      mutate(y = rep(1, 32), 
             cyl = as.factor(cyl)) %>%
      
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


# Project new data onto the new basis: -----
new.car <- data.frame(mpg = 10, 
                      cyl = 10, 
                      disp = 1000, 
                      hp = 400, 
                      drat = 5, 
                      wt = 2, 
                      qsec = 17, 
                      gear = 5, 
                      carb = 8, 
                      car = "newcar")

# todo: why not working? 
new.car[ ,-10] %>% 
      t %>% 
      scale(center = mtcars.pca$center)



#*************************************************
# save outputs --------
#*************************************************
ggsave(here("2018-11-24_mtcars-first-PC.pdf"), 
       p1.mtcars.pc1,
       width = 10)
