

#**********************************************************************
# IMPLEMENTING THE GAME OF LIFE IN R 
#**********************************************************************
# 2018-09-09
# Nayef Ahmad 

# Source: Intro to scientific programming and simulation using R, p83 
# http://www.tf.uns.ac.rs/~omorr/radovan_omorjan_003_prII/r-examples/spuRs/spuRs-2ed.pdf 



neighbours <- function(A, i, j, n) {
      # A is an n*n 0-1 matrix
      # calculate number of neighbours of A[i,j]
      
      # case when A[i, j] is not on the perimeter: 
      if (i > 1 && j > 1 && i < n && j < n){
            lower.row <- i - 1 
            upper.row <- i + 1 
            
            lower.col <- j - 1
            upper.col <- j + 1 
            
            sum <- A[lower.row, j] +  # top 
                  A[lower.row, lower.col] +  # top left 
                  A[lower.row, upper.col] + # top right 
                  A[i, lower.col] +  # left centre
                  A[i, upper.col] + # right centre
                  A[upper.row, j] +  # bottom 
                  A[upper.row, lower.col] + # bottom left 
                  A[upper.row, upper.col]  # bottom right 
            
            return(sum)
            
      } else {
            return(0)
      }
      
      
}



# grid size
n <- 50

# initialise lattice
A <- matrix(rep(0, n^2), n, n)
A[10, 10] <- 1
A[10, 11] <- 1
A[10, 12] <- 1


finished <- FALSE
while (!finished) {
      # plot
      plot(c(1,n), c(1,n), type = "n", xlab = "", ylab = "")
      for (i in 1:n) {
            for (j in 1:n) {
                  if (A[i,j] == 1) {
                        points(i, j)
                  }
            }
}

# update
B <- A
for (i in 1:n) {
      for (j in 1:n) {
            nbrs <- neighbours(A, i, j, n)
            if (A[i,j] == 1) {
                  if ((nbrs == 2) | (nbrs == 3)) {
                        B[i,j] <- 1
                  } else {
                        B[i,j] <- 0
                  }
            } else {
                  if (nbrs == 3) {
                        B[i,j] <- 1
                  } else {
                        B[i,j] <- 0
                  }
            }
      }
}
A <- B

## continue?
input <- readline("stop? ")
if (input == "y") finished <- TRUE
}
