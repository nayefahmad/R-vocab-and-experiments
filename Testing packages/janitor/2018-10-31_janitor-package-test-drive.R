

#**********************************************************************************
# JANITORPACKAGE TEST DRIVE 
# 2018-10-31
#**********************************************************************************

library(janitor)
library(magrittr)

help(package = "janitor")

# create a df with dirty names: 
df1 <- data.frame(`first COLUMN` = rpois(50,10), 
                  Second = rpois(50, 1), 
                  thirdCol = rpois(50, 4), 
                  category = c(rep("A", 10), 
                               rep("B", 10), 
                               rep("C", 30)))

head(df1)
str(df1)

df2 <- df1 %>% 
      clean_names() %>% 
      adorn_totals("col") %>% 
      adorn_totals("row")

head(df2)
tail(df2)
str(df2)


# creating tabyls (more pipe friendly than table(), and can be used with janitor::adorn): 
tabyl(df1, category)

tabyl(mtcars, cyl)
tabyl(mtcars, cyl, gear)
tabyl(mtcars, cyl, gear) %>% adorn_totals("col") %>% adorn_totals("row") 

# illustrating show_na functionality:
my_cars <- rbind(mtcars, rep(NA, 11))
my_cars %>% tabyl(cyl)
my_cars %>% tabyl(cyl, show_na = FALSE)



# Calling on a single vector not in a data.frame:
val <- c("hi", "med", "med", "lo")
tabyl(val)
