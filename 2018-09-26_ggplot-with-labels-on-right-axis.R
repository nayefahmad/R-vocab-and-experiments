

#**********************************************************
# GGPLOT WITH LINE LABELS ON RIGHT AXIS 
#**********************************************************

library("tidyverse")
library("RColorBrewer")

# Reference: https://drsimonj.svbtle.com/label-line-ends-in-time-series-with-ggplot2 


# Orange dataset is preloaded
?Orange

df1.oranges <- Orange 

df1.oranges %<>%       
      select_all(tolower)  # select_all( ) takes a function as arg 

str(df1.oranges)
summary(df1.oranges)
head(df1.oranges)



# create a vector of the last (furtherst right) y-axis values for each group: -------
final.ages <- df1.oranges %>% 
      group_by(tree) %>% 
      top_n(1, age) %>%  
      # top_n is a convenient wrapper that uses filter() and min_rank() to 
      #     select the top or bottom entries in each group, ordered by age 
      
      # Each tree has a set of age values. This orders each row by decreasing age values, 
      #     then picks the last value
      
      
      # select(circumference)
      pull(circumference)  # compare with select( ), which would not return a vector

final.ages



# plotting with ggplot:------------ 
p1.trees <- df1.oranges %>% 
      ggplot(aes(x = age, 
                 y = circumference, 
                 colour = tree)) + 
      geom_line(); p1.trees

# add sec axis: 
p2.tree.sec.axis <- p1.trees + 
      scale_y_continuous(sec.axis = sec_axis(trans = ~ .)); p2.tree.sec.axis

# ?sec_axis
# the transf arg specifies a transformation formula. Here we set "trans = ~ .", 
#     meaning no transformation. Also try something like "trans = ~ . *5"


# Finishing touches, starting from scratch: ---------------
p3.labels <- df1.oranges %>% 
      ggplot(aes(x = age, 
                 y = circumference, 
                 colour = tree)) + 
      geom_line(size = 2, 
                alpha = .8) + 
      scale_y_continuous(sec.axis = sec_axis(trans = ~ ., 
                                             breaks = final.ages)) + 
      scale_x_continuous(expand = c(0,0)) + 
      scale_color_manual(values = brewer.pal(5, "Accent")) + 
      
      labs(title = "Orange trees getting bigger with age", 
           subtitle = "Orange dataset in R", 
           x = "Days old", 
           y = "Circumference (mm)") + 
      theme_minimal(base_size = 16); p3.labels






