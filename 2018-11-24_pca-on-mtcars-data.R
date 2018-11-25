

#************************************************
# PCA WITH MTCARS 
# 2018-11-14
#************************************************

library(tidyverse)
library(broom)
library(ggrepel)

# reference: https://www.datacamp.com/community/tutorials/pca-analysis-r 


# set up data --------
# PCA works best with numeric data
df1.mtcars.subset <- 
      mtcars %>% 
      select(-c(vs, am))


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
str(mtcars.pca)

print(mtcars.pca)

plot(mtcars.pca)

tidy(mtcars.pca)  # %>% View("tidy(PCA)")
# this seems to give the "coordinates"/"component scores"
# of each car along each PC (without specifying the 
# "factor scores" that specify the linear comb of the 
# orig variables that result in the principal components)



# let's plot the cars on PC1: 
tidy(mtcars.pca) %>% 
      filter(PC == 1) %>% 
      mutate(y = rep(1, 32)) %>% 
      left_join(df1.mtcars.subset, 
                by = c("row" = ))
      
      ggplot(aes(x = value, 
                 y = y, 
                 label = row)) + 
      geom_point() + 
      geom_text_repel(force = 50)  
      
      theme(lab)
