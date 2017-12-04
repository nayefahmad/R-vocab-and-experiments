

# Checking other distance measures: -----------


library("dplyr")


setwd("//vch.ca/departments/Projects (Dept VC)/Patient Flow Project/Coastal HSDA/2017 Requests/2017.09.15 Comparison of LGH units with Fraser Health/bin")
med.crosstab <- read.csv("CMGs-vs-units-crosstab.csv")
med.crosstab[is.na(med.crosstab)] <- 0

med.crosstab <- select(med.crosstab, -c(X, cmg))

# str(med.crosstab)
# summary(med.crosstab)
# head(med.crosstab)


# find profiles: 
med.crosstab.colsum <- apply(med.crosstab, 2, sum)
med.profiles <- sweep(med.crosstab, 
                      2, 
                      med.crosstab.colsum, 
                      FUN = "/")

# str(med.profiles)
# head(med.profiles$LGH.4E, 50)

# euclidean
eu.dist <- dist(t(med.profiles), 
                method = "euclidean", 
                diag = FALSE, 
                upper = FALSE) %>% as.matrix
write.csv(eu.dist, 
          file="med-units-euclidean-distances.csv", 
          row.names = TRUE)

# manhattan 
man.dist <- dist(t(med.profiles), 
              method = "manhattan", 
              diag = FALSE, 
              upper = FALSE) %>% as.matrix
write.csv(man.dist, 
          file="med-units-manhattan-distances.csv", 
          row.names = TRUE)





# ****************************************************
# SURGICAL UNITS -------------
# ****************************************************
surg.crosstab <- read.csv("CMGs-vs-units-crosstab_surgery.csv")
surg.crosstab[is.na(surg.crosstab)] <- 0

surg.crosstab <- select(surg.crosstab, -c(X, cmg))

str(surg.crosstab)
summary(surg.crosstab)
head(surg.crosstab)


# find profiles: 
surg.crosstab.colsum <- apply(surg.crosstab, 2, sum)
surg.profiles <- sweep(surg.crosstab, 
                      2, 
                      surg.crosstab.colsum, 
                      FUN = "/")

# str(surg.profiles)
# head(surg.profiles$LGH.6E, 50)
# head(surg.profiles$LGH.6W, 50)


# euclidean
eu.dist_surg <- dist(t(surg.profiles), 
                method = "euclidean", 
                diag = FALSE, 
                upper = FALSE) %>% as.matrix
write.csv(eu.dist_surg, 
          file="surg-units-euclidean-distances.csv", 
          row.names = TRUE)

# manhattan 
man.dist_surg <- dist(t(surg.profiles), 
                 method = "manhattan", 
                 diag = FALSE, 
                 upper = FALSE) %>% as.matrix
write.csv(man.dist_surg, 
          file="surg-units-manhattan-distances.csv", 
          row.names = TRUE)


