

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
      
      
      
      
}



# grid size
n <- 50

# initialise lattice
A <- matrix(round(runif(n^2)), n, n)

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
#input <- readline("stop? ")
#if (input == "y") finished <- TRUE
}
