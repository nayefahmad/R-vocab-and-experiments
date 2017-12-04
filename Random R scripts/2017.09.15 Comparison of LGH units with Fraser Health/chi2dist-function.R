

# function to find chi2 distances: 

chi2dist <- function(dataframe){
      dataframe.rowsum <- apply(dataframe, 1, sum)
      dataframe.colsum <- apply(dataframe, 2, sum)
      dataframe.sum <- sum(dataframe)
      
      dataframe.exp <- dataframe.rowsum %o%
            dataframe.colsum/dataframe.sum
      # this gives the dataframe of expected profile coordinates 
      
      chi2 <- sum((dataframe-dataframe.exp)^2/dataframe.exp) 
      # chi2 statistic
      inertia <- chi2/dataframe.sum
      
      dataframe.profiles <- dataframe/dataframe.rowsum  
      # actual profiles for rows 
      dataframe.colmass <- dataframe.colsum/dataframe.sum  
      # column masses aka avg row profile
      
      
      chi.dist.from.centroid <- 
            apply((t(dataframe.profiles) - 
                         dataframe.colmass)^2/dataframe.colmass, 
                  2, sum)  
      # arguments to apply() to indicate colsums 
      # we now have all the chi2 distances from the centroid.
      # Next, we get all chi2 distances between all pairs of row
      # profiles: 
      
      dataframe.profiles.with.avg.profile <- rbind(dataframe.profiles, 
                                                    dataframe.colmass)
      # rownames(dataframe.profiles.with.avg.profile)[6] <- "avg"
      
      # CALCULATE ALL CHI2 distances
      dist(sweep(dataframe.profiles.with.avg.profile, 
                 2, 
                 sqrt(dataframe.colmass), 
                 FUN="/"))
}



# test the functions: -----
# create test data 
readership <- data.frame(C1=c(5,18,19,12,3), 
                         C2=c(7,46,29,40,7), 
                         C3=c(2,20,39,49,16))
rownames(readership) <- paste0("E", 1:5)


# function call: 
chi2dist(readership)
