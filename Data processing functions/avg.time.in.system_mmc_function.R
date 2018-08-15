
#*********************************************************
# FUNCTION: calculate W, the steady-state avg time in system 
# for M/M/c queue 
#*********************************************************
# 2018-08-14
# Nayef Ahmad 

# function defn: ------------
avg.tis_mmc <- function(lambda, 
                        mu, 
                        c,  # num servers 
                        p_0.function = p_0.prob  # function used to calculate p_0 
                        ){
      
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu)
      
      W = 1/mu + 
            (((r^c)/(factorial(c) * (c*mu) * ((1-rho)^2))) * 
            p_0.function(lambda, mu, c))
      
      return(W)
      
}



# test fn: ---------------
# Example from Gross, p72
p_0.prob(6, 3, 3) # 1/9; this is correct 

avg.tis_mmc(6, 3, 3)  # 0.4814815 hours = 28.88889 minutes; this is correct


