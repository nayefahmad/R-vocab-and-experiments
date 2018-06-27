

#**************************************************************
# COLOUR PALETTES IN R 
#**************************************************************

library("tidyverse")
library("grDevices")
library("RColorBrewer")

# reference: 

# 2 main functions: ---------
# colorRamp: Take a palette of colors and return a function that 
# takes valeus between 0 and 1, indicating the extremes of the 
# color palette (e.g. see the gray() function)

# colorRampPalette: Take a palette of colors and return a 
# function that takes integer arguments and returns a vector of 
# colors interpolating the palette (like heat.colors() or topo.colors())

# colorRamp():----------------------
# interpolate colors between red and blue: 
palette <- colorRamp(c("red", "blue"))

# palette is a fn that takes a number between 0 and 1 
palette(0)  # this gives the colour red in RGB

# you can pass any number from 0 to 1 to palette( ) 
palette(1)  # blue 
palette(0.5)  # purple-ish 

# you can also pass a seq numbers: 
palette(seq(0, 1, len = 10))


# you can interpolate between more than 2 cols: 
pal <- colorRamp(c("red", "green", "blue"))
pal(0)
pal(.4)



# colourRampPalette( ): --------------------
pal <- colorRampPalette(c("red", "yellow"))

# pal is a fn that takes an integer arg specifying num of 
# interpolated cols to return: 

pal(2)  # just gives red and yellow 
#FFFF00 ==> in this format, first 2 characters specify red 
# value, then next 2 specify green, then blue (in hex format)


# return 10 cols between red and yellow: 
pal(10)




# testing with ggplot: --------
mtcars %>% 
      ggplot(aes(x = hp,
                 y = mpg)) + 
      geom_point(aes(col = as.factor(cyl)), 
                 size = 5) +
      scale_colour_manual(values = pal(3)) + 
      theme_classic()



# using RColorBrewer: -------------------
# the only real fn to learn is brewer.pal(numColors, "theme name") 

display.brewer.all()
# 3 categories of palettes: sequential, diverging, qualitative 


# let's use 3 cols from the "Set1" palette: 
brewer.pal(3, "Set1")

# if I want I can pass this to colorRampPalette to interpolate
#     between the 3 values chosen 


# testing with ggplot again: -----------------
mtcars %>% 
      ggplot(aes(x = hp,
                 y = mpg)) + 
      geom_point(aes(col = as.factor(cyl)), 
                 size = 5) +
      scale_colour_manual(values = brewer.pal(3, "Set1")) + 
      theme_classic()


