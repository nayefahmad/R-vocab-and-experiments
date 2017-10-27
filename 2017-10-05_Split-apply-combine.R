
# ************************************************
# Split-apply-combine problems, using dplyr, tidyr and purrr
# ************************************************

library("plyr")
library("dplyr")
library("purrr")


# ************************************************
# TODO: -----------
# > fix purrr code 
# ************************************************


# ************************************************
# Ex 1: InsectSprays data ---------------
# https://www.r-bloggers.com/a-quick-primer-on-split-apply-combine-problems/ 
# ************************************************

str(InsectSprays)
head(InsectSprays) 

# > Approach 1: split > lapply > (unlist): ------------
(count_by_spray <- split(InsectSprays$count, InsectSprays$spray))
# splits the vector of counts into components of a list, organized by levels of 
# spray types 
# e.g. splitting a data frame: 
# split(mtcars, mtcars$cyl) %>% str
# split(mtcars, mtcars$cyl)[[1]]

mean_by_spray <- lapply(count_by_spray, mean) %>% print 
mean_by_spray <- unlist(mean_by_spray) %>% print 


# > Approach 2: split > sapply OR vapply (directly gives vector output): ------------
(count_by_spray <- split(InsectSprays$count, InsectSprays$spray))
sapply(count_by_spray, mean)

vapply(count_by_spray, mean, numeric(1))
# vapply is similar to sapply, but has a pre-specified type of return value, 
# so it can be safer (and sometimes faster) to use.


# > Approach 3: tapply OR by OR aggregate: ------------
tapply(InsectSprays$count, InsectSprays$spray, mean)
# "Apply a function to each cell of a ragged array, that is to each (non-empty)
      # group of values given by a unique combination of the levels of 
      #certain factors." 
# Interpretation: tapply takes 2 vectors, *splits* the first according to levels of 
      # the second (thus creating a "ragged array"), then it *applies* a function to 
      # each component, then it *combines* the results back into a vector.

by(InsectSprays$count, InsectSprays$spray, mean)
# Function by is an object-oriented wrapper for tapply applied to data frames.
# output: An object of class "by", giving the results for each subset.

aggregate(count ~ spray, InsectSprays, mean)
# Splits the data into subsets, computes summary statistics for each, and 
# returns the result in a convenient form.

# another example: 
# aggregate(state.x77, list(Region = state.region), mean)
# > state.x77 is a data frame 
# > list(Region = state.region) specifies how to group rows together. Think of it 
#   as creating a column that states which region each state is in. 

## example with character variables and NAs
# testDF <- data.frame(v1 = c(1,3,5,7,8,3,5,NA,4,5,7,9),
#                      v2 = c(11,33,55,77,88,33,55,NA,44,55,77,99)) %>% print 
# by1 <- c("red", "blue", 1, 2, NA, "big", 1, 2, "red", 1, NA, 12) %>% print 
# by2 <- c("wet", "dry", 99, 95, NA, "damp", 95, 99, "red", 99, NA, NA) %>% print 
# aggregate(x = testDF, by = list(by1, by2), FUN = "mean")
# adds by1 and by2 as cols to testDF, then finds all combinations of by1 and by2,
# and groups testDF according to them, then applies summary function

# ?aggregate  # worth looking into for more examples 


# > Approach 4: using plyr: ----------------
ddply(InsectSprays, .(spray), summarise, mean.count = mean(count))
# outputs a df

dlply(InsectSprays, .(spray), summarise, mean.count = mean(count))
# outputs a list 


# > Approach 5: using dplyr --------------------
InsectSprays %>% group_by(spray) %>% summarize(mean.count = mean(count))

# counting rows with dplyr: 
count(InsectSprays, spray)


# > Approach 6: using purrr --------------------
# InsectSprays %>% 
#       split(.$spray) %>% 
#       select(count) %>% 
#       map(~mean)
