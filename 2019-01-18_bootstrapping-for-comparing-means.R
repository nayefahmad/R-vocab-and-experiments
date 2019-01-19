

#**************************************************************************
# Bootstrapping the sampling distribution of the difference between 2 means 
# 2019-01-19 
# Nayef 

#**************************************************************************

library(tidyverse)
library(rsample)
library(janitor)

# reference: 
# https://tidymodels.github.io/rsample/articles/Working_with_rsets.html 

# rename for convenience: 
df1.attrition <- attrition %>%   # factors that lead to employee attrition
      clean_names()

str(df1.attrition)


# Traditionally, the bootstrap has been primarily used to empirically determine
# the sampling distribution of a test statistic. Given a set of samples with
# replacement, a statistic can be calculated on each analysis set and the
# results can be used to make inferences (such as confidence intervals).



#********************************************************************
# 1) Rsample basics: ----------
#********************************************************************

# https://tidymodels.github.io/rsample/articles/Basics.html

# > 1.1) what's a resample? --------
# We define a resample as the result of a two-way split of a data set. For
# example, when bootstrapping, one part of the resample is a sample with
# replacement of the original data. The other part of the split contains the
# instances that were not contained in the bootstrap sample



# > 1.2) what's an rset object? -------- 
# The main class in the package (rset) is for a set or collection of resamples. 
# In 10-fold cross-validation, the set would consist of the 10 different 
# resamples of the original data.


# >> 1.2.1) individual resamples are "rsplit" objects: ------
# two partitions that comprise a resample: 
# 1) "analysis" data: those that we selected in the resample. For a bootstrap,
#     this is the sample with replacement. For 10-fold cross-validation, 
#     this is the 90% of the data. These data are often used to fit a model or 
#     calculate a statistic in traditional bootstrapping.

# 2) "assessment" data:  the section of the original data not covered by the 
#     analysis set. Often used to evaluate the performance of a model that was
#     fit to the analysis data.



# > 1.3) mtcars example: ------
set.seed(1)

bootstrap_resamples <- bootstraps(mtcars, 
                                  times = 10)

str(bootstrap_resamples, 
    max.level = 1)
# 10 obs. of  2 variables: "splits" and "id" 

# examine result: 
bootstrap_resamples  # not very useful 


# >> 1.3.1) metadata about a split: -----
first_resample <- bootstrap_resamples$splits[[1]]  
first_resample  # <32/15/32>
# 32 data points in the analysis set 
# 15 data points in the assessment set 
# 32 data points in the original data 

# >> 1.3.1) data contained within the split: -----
first_resample$data  # first bootstrap sample data 

# get just the analysis/assessment data: 
analysis(first_resample)  %>% str
assessment(first_resample) %>% str







#********************************************************************
# 2) differences in mean/median monthly income between genders: ------
#********************************************************************

# > 2.1) boxplot: -------
p1.boxplots <- df1.attrition %>% 
      ggplot(aes(x = gender, 
                 y = monthly_income)) + 
      geom_boxplot() + 
      
      stat_summary(fun.y = mean, 
                   col = "firebrick", 
                   geom = "point") + 
      
      # data is positively skewed, so log it: 
      scale_y_log10(); p1.boxplots

# > 2.2) create resamples: ------------
boots.attrition <- bootstraps(df1.attrition, 
                              times = 50)



# > 2.3) compare medians: -------------

# If we wanted to compare the genders, we could conduct a t-test or rank-based
# test. Instead, let’s use the bootstrap to see if there is a difference in the
# median incomes for the two groups. We need a simple function to compute this
# statistic on the resample

# >> 2.3.1) function to calc difference in medians: ---------

# fn definition: 
median_diff_fn <- function(splits, FUN = round, ...){
      
      #**************************************************************
      # inputs: 
      # > an "rsplit" object 
      # > function to call on the data (e.g. mean/median)
      #     DO NOT PUT QUOTES AROUND NAME OF FUNCTION! 
      
      # output: difference (male - female) in statistics for the male 
      #     group vs the female group 
      
      # example function call: 
      # median_diff_fn(split, mean)  # to find mean
      
      #**************************************************************
      
      # get the analysis data from the rsplit object: 
      df <- analysis(splits)
      
      # find median for Male: 
      male <- df %>% filter(gender == "Male") %>% 
            pull(monthly_income) %>% 
            FUN()
      
      # find median for Female: 
      female <- df %>% filter(gender == "Female") %>% 
            pull(monthly_income) %>% 
            FUN()
      
      
      return(male-female)
      
}


# fn test: 
median_diff_fn(boots.attrition$splits[[1]], 
               mean)  # note that you can't put quotes around the function name





