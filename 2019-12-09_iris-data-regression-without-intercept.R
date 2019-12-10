
#'--- 
#' title: "Iris data: regression with and without intercept "
#' author: "Nayef Ahmad"
#' date: "2019-12-09"
#' output: 
#'   html_document: 
#'     keep_md: yes
#'     code_folding: show
#'     toc: true
#'     toc_float:
#'       collapsed: false
#'     toc_folding: false
#' ---

#+ lib, include = FALSE
library(tidyverse)
library(broom)
library(kableExtra)

#+ rest 
#' # Subset data for linear fit 
df <- iris %>% filter(Petal.Length > 2)
# str(df)

df %>% 
  ggplot(aes(x = Petal.Length, 
             y = Sepal.Length)) + 
  geom_point(alpha = 0.3) + 
  stat_smooth(method = lm, 
              fullrange = TRUE) + 
  coord_cartesian(xlim = c(0,10), 
                  ylim = c(0,10)) 
  

#' # Model without intercept 
m0 <- lm(Sepal.Length ~ Petal.Length - 1, 
         data = df)
summary(m0)

augment(m0) %>% 
  ggplot(aes(x = Sepal.Length, 
             y = .fitted)) + 
  geom_point(alpha = .5) + 
  coord_cartesian(xlim = c(0,10), 
                  ylim = c(0,10)) + 
  geom_abline(intercept = 0, 
              slope = 1)

tidy(m0) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                "condensed", 
                "responsive"))
              

#' # Model with intercept 
m1 <- lm(Sepal.Length ~ Petal.Length, 
         data = df)
summary(m1)


augment(m1) %>% 
  ggplot(aes(x = Sepal.Length, 
             y = .fitted)) + 
  geom_point(alpha = .5) + 
  coord_cartesian(xlim = c(0,10), 
                  ylim = c(0,10)) +
  geom_abline(intercept = 0, 
              slope = 1)


tidy(m1) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped",
                                      "condensed", 
                                      "responsive"))


#' # Discussion
#'
#' **Why does Rsq go down when adding a significant intercept term? **
#'
#' Ans. It doesn't really. Rsq and Adj-Rsq are almost always intended to be used
#' with an intercept in the model. When you drop the intercept, you're actually
#' calculating a different number, which is not directly comparable to the Rsq
#' for the models with intercept.
#'
#' See
#' [here](https://stats.stackexchange.com/questions/26176/removal-of-statistically-significant-intercept-term-increases-r2-in-linear-mo)
#' for more.
#'
#' > It helps to recall what R2 is trying to measure. In the former case, it is
#' comparing your current model to the reference model that only includes an
#' intercept (i.e., constant term). In the second case, there is no intercept,
#' so it makes little sense to compare it to such a model. So, instead, $R2_0$
#' is computed, which implicitly uses a reference model corresponding to noise
#' only.
#'
#' **In general, never drop the intercept unless you have very good reason to.**
#'
#'
#' Side note: Given the above it's a little insane that in python `statsmodels`
#' does not include an intercept by default.
#' 
#' 
