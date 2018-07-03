

#*********************************************************
# Visualizing example trees from RandomForest 
#*********************************************************
# Created: 2018-07-03

library("randomForest")
library("ggplot2")
# library("broom")  # can't deal with random forest :( 

# help(package = "randomForest")

# rm(list = ls())
# reference: https://stats.stackexchange.com/questions/2344/best-way-to-present-a-random-forest-in-a-publication 


# create rforest to predict mpg using all other vars: -----------
mtcars.rforest <- randomForest(mpg ~ ., 
                               data = mtcars, 
                               ntree = 1000, 
                               keep.forest = TRUE, 
                               importance = TRUE)


# str(mtcars.rforest)  # not super-helpful
# summary(mtcars.rforest)  # not super-helpful



# plotting errors and var importance as trees grow: -----------------
plot(mtcars.rforest)
plot(mtcars.rforest, log = "y")  # not sure what the point of log is 
# error units in miles per gallon? 

# ?varImpPlot
varImpPlot(mtcars.rforest)

pairs(mtcars) # look down first col - makes sense that disp, hp, wt are most important 

MDSplot(mtcars.rforest)  # todo: ??? 


# plotting single tree: --------------

# predict iris species using other vars: 
?getTree
getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)

getTree(mtcars.rforest, 
        sample(1:1000,1), 
        labelVar = TRUE)

# A matrix (or data frame, if labelVar=TRUE) with six columns and number of rows equal 
# to total number of nodes in the tree. For numerical predictors, data with 
# values of the variable less than or equal to the splitting point go to the 
# left daughter node.

# left daughter =  the row where the left daughter node is; 0 if the node is terminal
# right daughter	=  the row where the right daughter node is; 0 if the node is terminal
# split var	=  which variable was used to split the node; 0 if the node is terminal
# split point = where the best split is; see Details for categorical predictor
# status = is the node terminal (-1) or not (1)
# prediction = the prediction for the node; 0 if the node is not terminal


# using only 1 preditor: ---------------
mtcars.rforest2 <-  randomForest(mpg ~ wt, 
                                 data = mtcars, 
                                 ntree = 1000, 
                                 keep.forest = TRUE, 
                                 importance = TRUE)
plot(mtcars.rforest2)
getTree(mtcars.rforest2, 
        sample(1:1000,1), 
        labelVar = TRUE)

# repeat step above several times, see if first split is always around 
# the same value. Todo: write function/loop for this

# Ans. First split is usually between wt = 2 and wt = 4 

qplot(x = wt, y = mpg, data = mtcars) + theme_classic()

