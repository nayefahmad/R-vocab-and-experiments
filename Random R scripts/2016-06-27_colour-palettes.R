

#**************************************************************
# COLOUR PALETTES IN R 
#**************************************************************

library("tidyverse")
library("grDevices")

# reference: 

# 2 main functions: ---------
# colorRamp: Take a palette of colors and return a function that 
# takes valeus between 0 and 1, indicating the extremes of the 
# color palette (e.g. see the gray() function)

# colorRampPalette: Take a palette of colors and return a 
# function that takes integer arguments and returns a vector of 
# colors interpolating the palette (like heat.colors() or topo.colors())


# interpolate colors between red and blue: ------
palette <- colorRamp(c("red", "blue"))
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
