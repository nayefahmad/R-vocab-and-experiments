
#'--- 
#' title: "Bootstrapping the sampling distribution of the difference between 2 means"
#' author: "Nayef Ahmad"
#' date: "2019-01-19"
#' output: 
#'   html_document: 
#'     toc: true
#'     toc_float: true
#'     toc_depth: 5
#'     code_folding: "show"
#' ---

#' Reference: https://tidymodels.github.io/rsample/articles/Working_with_rsets.html  

#+ libraries, message = FALSE 
library(tidyverse)
library(rsample)
library(janitor)
library(kableExtra)


# rename for convenience: 
df1.attrition <- attrition %>%   # factors that lead to employee attrition
      clean_names()

# str(df1.attrition)


#' Traditionally, the bootstrap has been primarily used to empirically determine
#' the sampling distribution of a test statistic. Given a set of samples with
#' replacement, a statistic can be calculated on each analysis set and the
#' results can be used to make inferences (such as confidence intervals).


#' 
#' ### 1) `Rsample` basics

#' https://tidymodels.github.io/rsample/articles/Basics.html

#' \  
#' 
#' #### What's a *resample*? 
#' We define a *resample* as the result of a two-way split of a data set. For
#' example, when bootstrapping, one part of the resample is a sample with
#' replacement of the original data. The other part of the split contains the
#' instances that were not contained in the bootstrap sample
#' 
#' Here, each resample has class `rsplit` - see below. 


#' \  
#' 
#' #### What's an `rset` object?
#'
#' The main class in the package (`rset`) is for a set or collection of
#' resamples (aka `rsplit`s). In 10-fold cross-validation, the set would consist
#' of the 10 different resamples of the original data.

#' \  
#' 
#' #### Individual resamples are `rsplit` objects
#'
#' There are two partitions that comprise an `rsplit`:
#'
#' 1. *"analysis"* data: those that we selected in the resample. For a bootstrap,
#' this is the sample with replacement. For 10-fold cross-validation, this is
#' the 90% of the data. These data are often used to fit a model or calculate a
#' statistic in traditional bootstrapping.
#' 

#' 2. *"assessment"* data:  the section of the original data not covered by the
#' analysis set. Often used to evaluate the performance of a model that was fit
#' to the analysis data.


#' \  
#' \    
#' 
#' ### 2) `mtcars` example
set.seed(1)

#' \   
#'  
#' #### Bootstrapping mtcars dataset: 
#' 

bootstrap_resamples <- bootstraps(mtcars, 
                                  times = 10)

str(bootstrap_resamples, 
    max.level = 1)
# 10 obs. of  2 variables: "splits" and "id" 

# examine result: 
bootstrap_resamples  # not very useful 

#' \  
#' 
#' #### Metadata about a split: 
first_resample <- bootstrap_resamples$splits[[1]]  
first_resample  # <32/11/32>

#'
#' We learn the following about this particular resample: 
#'
#' * 32 data points in the analysis set 
#' * 11 data points in the assessment set 
#' * 32 data points in the original data 

#' \ 
#'  
#' #### Data contained within the split: 
first_resample$data  # first bootstrap sample data 

#' \  
#' 
#' #### Get just the analysis/assessment data: 
analysis(first_resample)  %>% str  # get the "analysis"/training data 
assessment(first_resample) %>% str  # # get the "assessment"/test data 






#' \  
#' \  
#' \  
#' 
#' ### 3) Compare *median* monthly income between genders
#' 
#' \  
#' 

#' #### boxplot: 
p1.boxplots <- df1.attrition %>% 
      ggplot(aes(x = gender, 
                 y = monthly_income)) + 
      geom_boxplot() + 
      
      stat_summary(fun.y = mean, 
                   col = "firebrick", 
                   geom = "point") + 
      
      # data is positively skewed, so log it: 
      scale_y_log10(); p1.boxplots

#' \  
#' 
#' #### Create resamples: 

# bootstrapping attrition dataset 
boots.attrition <- bootstraps(df1.attrition, 
                              times = 500)


#' \  
#' 
#' #### Compare medians between genders: 
#' 

#' If we wanted to compare the genders, we could conduct a t-test or rank-based
#' test (e.g. Wilcoxon's). Instead, let’s use the bootstrap to see if there is a
#' difference in the median incomes for the two groups. We need a simple
#' function to compute this statistic on the resample
#' 
#' \  
#' 

#' #### Function to calc difference in medians: 

# fn definition: 
compare_male_female_stat <- function(splits, FUN = median, ...){
      
      #**************************************************************
      # inputs: 
      # > an "rsplit" object (i.e. single data point in the "splits" column that
      #     results from calling bootstraps() on a dataset) 
      # > function to call on the data (e.g. mean/median)
      #     DO NOT PUT QUOTES AROUND THE NAME OF THE FUNCTION! 
      
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
compare_male_female_stat(boots.attrition$splits[[1]], 
                         mean)  # note that you can't put quotes around the function name


#' \  
#' 
#' #### Map function across all splits: 
# create a new col in the object boots.attrition: 

boots.attrition <- boots.attrition %>% 
  mutate(diff_median = map_dbl(boots.attrition$splits,  # each rsplit is passed to the function compare_male_female_stat(), and bound to the formal argument "splits" 
                               compare_male_female_stat))

boots.attrition


#' \  
#' 
#' #### Plot distribution of difference in medians
#'
#' Even though you actually only have one sample for males and one sample for
#' females, bootstrapping allows to to "pretend" that we actually have 500
#' samples of each, so that we could calculate the difference in means 500
#' times, and see what its distribution looks like.
#' 
boots.attrition %>% 
      ggplot(aes(x = diff_median))+
      geom_density() + 
      geom_vline(xintercept = 0, 
                 colour = "red") + 
      geom_vline(xintercept = boots.attrition$diff_median %>% mean, 
                 col = "grey70") + 
      labs(title = "diff in medians (male - female)") 

#' \  
#' 
#' #### Interpretation of sampling distribution:  
#' 

#' The sampling distribution is centered around -250 (grey line in the graph above).
#'  However, we know that the center of the sampling distribution of Y-bar = (median(male) - median(female)) 
#' is not necessarily the center of the population distribution of the 
#' parameter Y. 
#' 

#' However, the variance of the sampling dist. also allows us to estimate
#' variance of the population.
#' 

#' Putting these 2 pieces of info together (the center of sampling dist, and the
#' variance of sampling dist.), we can construct a 95% confidence interval for
#' the population parameter Y. CIs contructed in this way are guaranteed to
#' capture the true population parameter 95% of the time.


#' \  
#' 
#' ### 4) Compare *means* between genders: 
boots.attrition <- boots.attrition %>% 
  mutate(diff_mean = map_dbl(boots.attrition$splits, 
                             compare_male_female_stat, 
                             FUN = mean))

boots.attrition 
                

# plot diff: 
boots.attrition %>% 
      ggplot(aes(x = diff_mean))+
      geom_density() + 
      geom_vline(xintercept = 0, 
                 colour = "red") + 
      labs(title = "diff in means (male - female)")


#' \  
#'
#' ### 5) Compare max between genders: 
boots.attrition$diff_max <- map_dbl(boots.attrition$splits, 
                                     compare_male_female_stat, 
                                     FUN = max)


# plot diff: 
boots.attrition %>% 
      ggplot(aes(x = diff_max))+
      geom_density() + 
      geom_vline(xintercept = 0, 
                 colour = "red") + 
      labs(title = "diff in max (male - female)")

#'
#' **Question**: Do the relative heights give an estimate of the prob that the
#' highest paid person will be male vs female? E.g. if the male spike is 6 and
#' the female one is 4, does this mean there's a 60% chance that in the
#' population as a whole, the highest paid person is male?
#' 
#' \  
#' \  