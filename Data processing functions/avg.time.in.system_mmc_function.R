
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
      
      # output: average time in system (TIS) including time in queue and 
      #     time in service 
      # Note: for ED modelling, if we define service time as start to 
      #     disposition time, then we expect most of the TIS to fall under time in service 
      #     The division between "queue" and "service" can be arbitrarily selected for this system 
      
      # define vars: 
      r <-  lambda/mu 
      rho <- lambda/(c*mu)
      
      W = 1/mu + 
            (((r^c)/(factorial(c) * (c*mu) * ((1-rho)^2))) * 
            p_0.function(lambda, mu, c))
      
      return(W)
      
}


#********************************************
# test the fn: ---------------
#********************************************

# > Example from Gross, p72: ----------
p_0.prob(6, 3, 3) # 1/9; this is correct 

avg.tis_mmc(6, 3, 3)  # 0.4814815 hours = 28.88889 minutes; this is correct




# > Examples from VCH data: ----------

# todo: obviously, all of the below should be done in a purrr:pmap( ) loop 
# Example: VGH, calendar 2017  
avg.tis_mmc(267, 4.08, 143) * 24  # 5.882353 hours 
# data from cube: 5.89 hours. 
# This is effectively equal! <1 minute difference!

# note: c*mu = 4.08*143 = 583.44; if lambda reaches this value, system breaks down!!


# Example: RHS, calendar 2017   
avg.tis_mmc(156, 4.25, 67) * 24  # 5.64706 hours 
# data from cube: 5.65 hours. 
# This is effectively equal! <1 minute difference!


# Example: LGH, calendar 2017    
avg.tis_mmc(176, 5.19, 154) * 24  # 4.624277 hours 
# data from cube: 4.63 hours. 
# This is effectively equal! <1 minute difference!


# Example: SPH, calendar 2017    
avg.tis_mmc(242, 4.57, 64) * 24  # 5.297288 hours 
# data from cube: 5.25 hours. 
# Pretty damn close: 3 minutes difference (<1% diff)


# Example: MSJ, calendar 2017    
avg.tis_mmc(88, 8.11, 45) * 24  # 2.959309 hours 
# data from cube: 2.96 hours. 
# This is effectively equal! <1 minute difference!


# Example: UBC, calendar 2017    
avg.tis_mmc(60, 10.45, 79) * 24  # 2.296651 hours 
# data from cube: 2.30 hours. 
# This is effectively equal! <1 minute difference!


# Example: VCH ex C.Rural, calendar 2017 
avg.tis_mmc(989, 4.81, 552) * 24  # todo: c=552 is too large to compute factorial! 



#***********************************************************
# Example: VGH, calendar 2016
avg.tis_mmc(262, 4.13, 143) * 24  # 5.811138 hours 
# data from cube: 5.81 hours. 
# This is effectively equal! <1 minute difference!


# Example: SPH, calendar 2016
avg.tis_mmc(237, 4.38, 64) * 24  # 5.552992 hours 
# data from cube: 5.48 hours. 
# This is pretty close: 4.4 minute differencet (~1.3% diff)


# Example: LGH, calendar 2016
avg.tis_mmc(177, 4.07, 154) * 24  # 5.896806 hours 
# data from cube: 5.89 hours. 
# This is effectively equal! <1 minute difference!


# Example: RHS, calendar 2016
avg.tis_mmc(154, 4.22, 67) * 24  # 5.687205 hours 
# data from cube: 5.68 hours. 
# This is effectively equal! <1 minute difference!


# Example: MSJ, calendar 2016
avg.tis_mmc(85, 7.97, 45) * 24  # 3.011292 hours 
# data from cube: 3.01 hours. 
# This is effectively equal! <1 minute difference!


# Example: UBC, calendar 2016
avg.tis_mmc(59, 11.63, 79) * 24  # 2.063629 hours 
# data from cube: 32.06 hours. 
# This is effectively equal! <1 minute difference!


