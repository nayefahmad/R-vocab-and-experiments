

library("ca")  # help(package="ca")
# library("reshape2")
library("dplyr")
# library("zoom")
library("ggplot2")
library("ggrepel")

# getwd()
# rm(list=ls())

# **************
# TODO ------
# add rownames as col for plotting 

# **************



# example of CA: -------
smoke  # dataset
ca(smoke)
plot(ca(smoke))
plot(ca(smoke), map="rowprincipal")
plot(ca(smoke), map="colprincipal")


# ********************************
# Example 2: Calculating distances between profiles  ----------
# ********************************
readership <- data.frame(C1=c(5,18,19,12,3), 
                    C2=c(7,46,29,40,7), 
                    C3=c(2,20,39,49,16))
rownames(readership) <- paste0("E", 1:5)

# str(readership)
# summary(readership)

# > plotting the easy way: ----
ca(readership)
# asymmetric rowprincipal 
p1 <- plot(ca(readership), map="rowprincipal"); p1 
p2 <- plot(ca(readership), map="colprincipal"); p2
p3 <- plot(ca(readership)); p3 # SYMMETRIC MAP 

# notice that p1 has rows in  principal coordinates and cols in 
# standard coords. p2 has cols in principal coords, rows in standard coords. 
# p3 combines p1 and p2, so that both rows and cols have principal 
# coordinates. 

# > using cacoord() to get coordinates: ----
cacoord(ca(readership))  # default is to get standard coords for both 
                        # rows and cols 
cacoord(ca(readership), type="standard")  # standard coords for both 
                        # rows and cols 

# checking equality: 
# as.data.frame(cacoord(ca(readership))[1])== 
#       as.data.frame(cacoord(ca(readership), type="standard")[1])
# as.data.frame(cacoord(ca(readership))[2])== 
#       as.data.frame(cacoord(ca(readership), type="standard")[2])

cacoord(ca(readership), type="principal")  # principal coords for both 
                        # rows and cols (SYMMETRIC MAP)

# > calculating distances manually ------
readership.rowsum <- apply(readership, 1, sum)
readership.colsum <- apply(readership, 2, sum)
readership.sum <- sum(readership)

readership.exp <- readership.rowsum %o% readership.colsum/readership.sum
# this gives the readership of expected profile coordinates 

chi2 <- sum((readership-readership.exp)^2/readership.exp)  # chi2 statistic
inertia <- chi2/readership.sum

readership.profiles <- readership/readership.rowsum  # actual profiles for rows 
readership.colmass <- readership.colsum/readership.sum  # column masses aka avg row profile


chi.dist.from.centroid <- 
      apply((t(readership.profiles) - 
                   readership.colmass)^2/readership.colmass, 
            2, sum)  # arguments to apply() to indicate colsums 
# we now have all the chi2 distances from the centroid. Next, we get 
# all chi2 distances between all pairs of row profiles: 

readership.profiles.with.avg.profile <- rbind(readership.profiles, 
                                         readership.colmass)
rownames(readership.profiles.with.avg.profile)[6] <- "avg"

# CALCULATE ALL CHI2 distances
dist(sweep(readership.profiles.with.avg.profile, 
           2, 
           sqrt(readership.colmass), 
           FUN="/"))
# calling dist on readership.profiles.with.avg.profile would give 
# Euclidean distances, which we don't want. So, in the command 
# sweep() above, we first transform profiles in such a way that 
# calculating Euclidean distances between transformed profiles 
# gives chi2 distances between original profiles. 

# > let's try getting the point coords manually : -----
readership.P    <- readership/sum(readership)
readership.r    <- apply(readership.P, 1, sum)
readership.c    <- apply(readership.P, 2, sum)
readership.Dr   <- diag(readership.r)
readership.Dc   <- diag(readership.c)
readership.Drmh <- diag(1/sqrt(readership.r))
readership.Dcmh <- diag(1/sqrt(readership.c))

readership.P   <- as.matrix(readership.P)
readership.S   <- readership.Drmh%*%(readership.P-readership.r%o%readership.c)%*%readership.Dcmh
readership.svd <- svd(readership.S)


readership.rsc <- readership.Drmh%*%readership.svd$u
readership.csc <- readership.Dcmh%*%readership.svd$v
readership.rpc <- readership.rsc%*%diag(readership.svd$d)
readership.cpc <- readership.csc%*%diag(readership.svd$d)


# exampine coordinates: 
readership.rsc  # coords of rows in colprincipal analysis 
readership.csc  # coords of cols in rowprincipal analysis 
readership.rpc  # coords of rows in rowprincipal analysis: 
readership.cpc  # coords of cols in colprincipal analysis: 


# > plotting with ggplot: ------------
# assymetric rowprincipal map: 
row.coords <- as.data.frame(cacoord(ca(readership),
                                    type="principal")[1])
col.coords <- as.data.frame(cacoord(ca(readership),
                                    type="standard")[2])

row.coords <- mutate(row.coords, 
                     label=paste0("E", 1:5))
col.coords <- mutate(col.coords, 
                     label=paste0("C", 1:3))

colnames(row.coords) <- c("dim1", "dim2", "label")
colnames(col.coords) <- c("dim1", "dim2", "label")


p4_with_labels <- 
      ggplot() + 
      geom_point(data=row.coords, 
                 aes(x=dim1, y=dim2), 
                 shape =1) + 
      geom_point(data=col.coords, 
                 aes(x=dim1, y=dim2)) + 
      
      geom_hline(yintercept = 0) + 
      geom_vline(xintercept = 0) + 
      
      # add text labels: 
      geom_text_repel(data=row.coords, 
                      aes(x=dim1, y=dim2, 
                          label=label), 
                      size=5, 
                      col="red") + 
      geom_text_repel(data=col.coords, 
                      aes(x=dim1, y=dim2, 
                          label=label), 
                      size=5, 
                      col="blue") + 
      theme_classic(base_size = 16); p4_with_labels


