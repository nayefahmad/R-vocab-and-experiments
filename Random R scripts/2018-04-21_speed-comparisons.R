

# =============================================
# Modifying a data frame in place
# =============================================

library(ggplot2)
str(diamonds)

# Modify data frame in place, in loop
# This is super, super slow
system.time({
      d <- diamonds
      for (i in 1:length(d$carat)) d$carat[i] <- i
})

# str(d)

# Modify separate vector in loop, then assign back into data frame
# Much faster! 
system.time({
      d <- diamonds
      carat <- d$carat
      for (i in 1:length(carat)) carat[i] <- i
      
      d$carat <- carat
})

# Modify separate, growing vector in loop, then assign back into data frame
# Not too bad
system.time({
      d <- diamonds
      carat <- numeric()
      for (i in 1:nrow(d)) carat[i] <- i
      
      d$carat <- carat
})

# Use within() to modify data frame
system.time({
      d <- within(diamonds, {
            for (i in 1:length(carat)) carat[i] <- i
      })
})

# Converting data frame to list and doing assignment is much faster
system.time({
      d <- as.list(diamonds)  
      # each col is a separate element of the list
      
      for (i in 1:length(d$carat)) d$carat[i] <- i
      d <- as.data.frame(d)
})

# Vectorized assignment
# SUPERFAST! 
system.time({
      d <- diamonds
      d$carat <- 1:length(d$carat)
})


# =============================================
# For loops, lapply, and vapply
# =============================================

# Grow a vector in place with a for loop
system.time({
      v <- numeric()
      for (i in 1:1e5) v[i] <- i*2
})

# lapply (returns list - need to convert to atomic vector)
system.time({
      v <- lapply(1:1e5, function(x) x*2)
      v <- unlist(v)
})



# Modify pre-allocated vector in place with a for loop
# Note this is 10x larger than previous examples
system.time({
      v <- numeric(1e6)
      for (i in 1:1e6) v[i] <- i*2
})

# lapply (returns list - need to convert to atomic vector)
system.time({
      v <- lapply(1:1e6, function(x) x*2)
      v <- unlist(v)
})

# vapply (returns atomic vector)
system.time({
      v <- vapply(1:1e6, function(x) x*2, FUN.VALUE = numeric(1))
})

# vectorized computation
system.time({
      v <- (1:1e6) * 2
})


# =============================================
# Modifying lists in place
# =============================================

# Grow a list in place with a for loop
system.time({
      v <- list()
      for (i in 1:1e5) v[[i]] <- i*2
})

# lapply without pre-allocations: 
# this is actually slower! 
system.time({
      v <- lapply(1:1e5, function(x) x*2)
})

# Modify pre-allocated list in place with lapply 
# very slightly faster 
system.time({
      v <- as.list(numeric(1e5))
      v <- lapply(1:1e5, function(x) x*2)
})




# Modify pre-allocated list in place with a for loop
system.time({
      v <- as.list(numeric(1e5))
      for (i in 1:1e5) v[[i]] <- i*2
})

# Modify pre-allocated vector in place with a for loop
system.time({
      v <- numeric(1e5)
      for (i in 1:1e5) v[[i]] <- i*2
})

