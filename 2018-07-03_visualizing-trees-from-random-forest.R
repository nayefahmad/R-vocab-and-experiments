

#*********************************************************
# Visualizing example trees from RandomForest 
#*********************************************************
# Created: 2018-07-03

library("randomForest")
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
# to total number of nodes in the tree. 
# 
# left daughter =  the row where the left daughter node is; 0 if the node is terminal
# right daughter	=  the row where the right daughter node is; 0 if the node is terminal
# split var	=  which variable was used to split the node; 0 if the node is terminal
# split point = where the best split is; see Details for categorical predictor
# status = is the node terminal (-1) or not (1)
# prediction = the prediction for the node; 0 if the node is not terminal
