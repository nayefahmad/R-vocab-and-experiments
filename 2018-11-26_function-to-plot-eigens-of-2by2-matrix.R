

#*************************************************************************
# FUNCTION TO PLOT THE EIGENVECTORS OF A GIVEN 2X2 MATRIX 
#*************************************************************************

# function defn: ---------------------
plot.eigen_function <- function(input.matrix){
      
      # input: a 2x2 matrix 
      # output: a plot showing the eigenvectors and eigenvalues of the matrix
      
      library(ggplot2)
      
      eigens <- eigen(input.matrix)
      
      plotdata <- 
            data.frame(x = c(eigens$vectors[1,1],  # x coord of eigen 1 
                             0, 
                             -eigens$vectors[1,1],  # neg of x coord of eigen 1
                             
                             eigens$vectors[1,2],  # x coord of eigen 2 
                             0, 
                             -eigens$vectors[1,2]), # neg of x coord of eigen 2
                       
                       
                       y = c(eigens$vectors[2,1],  # y coord of eigen1
                             0, 
                             -eigens$vectors[2,1], # neg of y coord of eigen 1 
                             
                             eigens$vectors[2,2], 
                             0, 
                             -eigens$vectors[2,2]), 
                       group = factor(c(rep("eigenvector1", 3), 
                                        rep("eigenvector2", 3))))
      
      # get input matrix as string: 
      input.string <- paste0("Input matrix: [[", 
                             input.matrix[1, 1], 
                             ", ", 
                             input.matrix[2, 1], 
                             "], [", 
                             input.matrix[1, 2], 
                             ", ", 
                             input.matrix[2, 2], 
                             "]]")
      
      
      # setting x and y scale: 
      x.scale <- dist(matrix(c(input.matrix[1, 1],
                               input.matrix[2,1], 
                               0, 
                               0), 
                             byrow = TRUE, 
                             nrow = 2))
      x.scale <- ceiling(x.scale)
      
      
      
      
      # plot graph: 
      ggplot(plotdata, 
             aes(x = x, 
                 y = y, 
                 col = group, 
                 group = group)) + 
            geom_line(size = 2) + 
            
            # scale_x_continuous(limits = c(-x.scale, x.scale)) +
            # scale_y_continuous(limits = c(-x.scale, x.scale)) +
            
            geom_hline(yintercept = 0) + 
            geom_vline(xintercept = 0) + 
            
            labs(title = input.string)
      
      # return(plotdata)      
      
}




# function test: --------------------
m1 <- matrix(c(2, 1, 1, 1),
             ncol = 2); m1

m2 <- matrix(c(60, 13, 13, 12),
             ncol = 2); m2

m3 <- matrix(c(2000, -100, -100, 40),
             ncol = 2); m3

m4 <- matrix(c(36, -320.7321, -320.7321, 4700),
             ncol = 2); m4


plot.eigen_function(m1)
plot.eigen_function(m2)
plot.eigen_function(m3)
plot.eigen_function(m4)


