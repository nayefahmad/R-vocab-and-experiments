
# data for different treatments: 
data.df <- data.frame(treatment = as.factor(c("x1", "x2")),
                      volume = c(1000, 50))
str(data.df)

# historical data for how long these treatments take: 
duration.distribution <- list(x1 = c(1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 3), 
                              x2 = c(0.5, 0.5, 0.5, 1, 10)) 

# function to simulate  total time for each treatment:
treatment.time <- function(treatment, volume){
      treatment <- as.character(treatment)
      full.data <- sample(duration.distribution[[treatment]], 
                          volume, 
                          replace = TRUE)
      return(sum(full.data))
}


# now call the function, first with hard-coded values, then with mapply: 
treatment.time("x1", 1000)
treatment.time(data.df$treatment[1], data.df$volume[1])

treatment.time("x2", 50)
treatment.time(data.df$treatment[2], data.df$volume[2])

# use mapply: 
mapply(treatment.time, 
       data.df$treatment, 
       data.df$volume)
