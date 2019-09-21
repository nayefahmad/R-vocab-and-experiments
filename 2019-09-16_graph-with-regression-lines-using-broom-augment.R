
library(tidyverse)
library(broom)

# prep data: 
mtcars.modified <- 
  mtcars %>% 
  mutate(cyl = as.factor(cyl))

# fit model: 
m1 <- lm(mpg ~ hp + cyl, 
         data = mtcars.modified)

# plot regression lines: 
m1 %>% 
  broom::augment() %>%  # add columns with fitted values, residuals, etc. 
  
  ggplot() + 
  
  # add the fitted regresseion lines: 
  geom_line(aes(x = hp, y = .fitted, group = cyl, col = cyl)) + 
  
  # add the actual y-values
  geom_point(data = mtcars.modified, 
             aes(x = hp, y = mpg, col = cyl), 
             alpha = .2) 
