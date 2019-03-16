

#*************************************************************************
# DE-SEASONALIZING NUM ACUTE ADMITS DATA and RUNNING SEGMENTED REGRESSION ANALYSIS
# 2018-12-24
# Nayef 
#*************************************************************************

library(forecast)
library(fpp)
library(ggplot2)
library(tidyverse)
library(here)
library(janitor)
library(TSA)
library(ggpubr)
library(broom)


# rm(list = ls())
source(here("src", 
            "stl.as.df_function.R"))


# 1) input dataset - acute admits: -------------------
df1.orig.data <- read_csv(here("data", 
                               "2019-01-30_vgh_purdy-pavilion-intervention-acute-admits.csv")) %>% 
      clean_names()


str(df1.orig.data)


# 2.1) create pre-intervention ts: -------------------
ts1.pre.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 0) %>% 
      pull(acute_admits) %>% 
      ts(start = c(2015, 1), 
         frequency = 12)

ts1.pre.intervention

# 2.2) create POST-intervention ts: -------------------
ts2.post.intervention <- 
      df1.orig.data %>% 
      filter(is_post_intervention == 1) %>% 
      pull(acute_admits) %>% 
      ts(start = c(2017, 1), 
         frequency = 12)

ts2.post.intervention






# 3) Fit models to pre-intervention series: -------------
# > 3.1) model 1: trend only --------------

m1.pre.trend <- tslm(ts1.pre.intervention ~ trend) 
summary(m1.pre.trend)  # no significant trend 

# plot data and trend: 
p1.data.and.trend <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 trend = predict(m1.pre.trend), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "val", 
             -period) %>% 
      ggplot(aes(x = period, 
                 y = val, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      theme(legend.position = "none"); p1.data.and.trend



# > 3.2) model 2: approximate the seasonal pattern using Fourier terms -------

m2.fourier <- tslm(ts1.pre.intervention ~ trend + fourier(ts1.pre.intervention,2))
summary(m2.fourier)

# save coefficients: 
df2.coeffients.from.m2 <- tidy(m2.fourier)


# what does the sum of all these terms look like? 
sum.of.fouriers <- fourier(ts1.pre.intervention, 2) %>%
      as.data.frame() %>% 
      apply(MARGIN = 1, 
            FUN = sum)

# >> plot sum of fourier terms: 
p2.fourier.terms <- 
      data.frame(period = rep(1:24), 
                 value = sum.of.fouriers) %>% 
      ggplot(aes(x = period, 
                 y = value)) +
      geom_hline(yintercept = 0, 
                 col = "grey60") + 
      geom_line(col = "coral2"); p2.fourier.terms

# interestingly, this is very similar to the pattern for the ED visits series

# >> compare with original series: 
ggarrange(p1.data.and.trend, 
          p2.fourier.terms, 
          nrow = 2)


# now let's add in the final trend + fourier series: 
p3.final.series <- 
      data.frame(data = as.numeric(ts1.pre.intervention), 
                 predicted.with.fourier = predict(m2.fourier), 
                 period = 1:24) %>% 
      gather(key = "key", 
             value = "value", 
             -period) %>%  
      
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 col = key)) + 
      geom_line() + 
      theme(legend.position = "bottom"); p3.final.series


# compare predictions from model m1 vs m2: 
ggarrange(p1.data.and.trend, 
          p3.final.series, 
          nrow = 2)



# 3.3) decomposition into trend/season/remainder: -----

# first let's create the trend series from model m2: 
pre.trend.m2 <- 
      df2.coeffients.from.m2$estimate[1] +  # intercept 
      df2.coeffients.from.m2$estimate[2] * seq_along(ts1.pre.intervention)


df3.pre.decomposed <- 
      cbind(data = ts1.pre.intervention, 
            trend = pre.trend.m2,                            # from model m2.fourier
            season = ts1.pre.intervention - pre.trend.m2 - resid(m2.fourier),  # from model m2.fourier
            remainder = resid(m2.fourier))               # from model m2.fourier

df3.pre.decomposed
str(df3.pre.decomposed)  
# note that this is not a df, so we can't use $ to subset columns. 
# e.g. to get teh season component, we write: 
# df2.decomposed[,'season']

# plot decomposed series: 
autoplot(df3.pre.decomposed, facets = TRUE)


# plot all components: 
autoplot(df3.pre.decomposed[, 'trend'], 
         series = "Trend") + 
      
      autolayer(df3.pre.decomposed[, 'season'], 
                series = "Seasonal") + 
      
      autolayer(df3.pre.decomposed[, 'remainder'], 
                series = "Remainder") + 
      
      autolayer(ts1.pre.intervention, 
                series = "Raw data") + 
      
      geom_hline(yintercept = 0) + 
      
      
      labs(title = "ED Visits: De-seasonalized trend pre-intervention", 
           subtitle = "Jan 2015 to Dec 2016")





# 4) Fit models to post-intervention series: -------------
# > 4.1) model 1: trend only --------------

m3.post.trend <- tslm(ts2.post.intervention ~ trend) 
summary(m3.post.trend)  # no significant trend 

# plot data and trend: 
p4.data.and.trend <- 
      data.frame(data = as.numeric(ts2.post.intervention), 
                 trend = predict(m3.post.trend), 
                 period = 1:20) %>% 
      gather(key = "key", 
             value = "val", 
             -period) %>% 
      ggplot(aes(x = period, 
                 y = val, 
                 group = key, 
                 colour = key)) + 
      geom_line() + 
      theme(legend.position = "none"); p4.data.and.trend



# > 4.2) model 2: approximate the seasonal pattern using Fourier terms -------

m4.post.fourier <- tslm(ts2.post.intervention ~ trend + fourier(ts2.post.intervention,2))
summary(m4.post.fourier)

# save coefficients: 
df4.coeffients.from.m4 <- tidy(m4.post.fourier)


# what does the sum of all these terms look like? 
sum.of.fouriers2 <- fourier(ts2.post.intervention, 2) %>%
      as.data.frame() %>% 
      apply(MARGIN = 1, 
            FUN = sum)

# >> plot sum of fourier terms: 
p5.fourier.terms <- 
      data.frame(period = rep(1:20), 
                 value = sum.of.fouriers2) %>% 
      ggplot(aes(x = period, 
                 y = value)) +
      geom_hline(yintercept = 0, 
                 col = "grey60") + 
      geom_line(col = "coral2"); p5.fourier.terms
# compare p5 with p2 ==> same seasonal pattern detected


# >> compare with original series: 
ggarrange(p4.data.and.trend, 
          p5.fourier.terms, 
          nrow = 2)


# now let's add in the final trend + fourier series: 
p6.post.final.series <- 
      data.frame(data = as.numeric(ts2.post.intervention), 
                 predicted.with.fourier = predict(m4.post.fourier), 
                 period = 1:20) %>% 
      gather(key = "key", 
             value = "value", 
             -period) %>%  
      
      ggplot(aes(x = period, 
                 y = value, 
                 group = key, 
                 col = key)) + 
      geom_line() + 
      theme(legend.position = "bottom"); p6.post.final.series


# compare predictions from model m1 vs m2: 
ggarrange(p4.data.and.trend, 
          p6.post.final.series, 
          nrow = 2)



# 4.3) decomposition into trend/season/remainder: -----

# first let's create the trend series from model m2: 
post.trend.m4 <- 
      df4.coeffients.from.m4$estimate[1] +  # intercept 
      df4.coeffients.from.m4$estimate[2] * seq_along(ts2.post.intervention)


df5.post.decomposed <- 
      cbind(data = ts2.post.intervention, 
            trend = post.trend.m4,                            # from model m2.fourier
            season = ts2.post.intervention - post.trend.m4 - resid(m4.post.fourier),  # from model m2.fourier
            remainder = resid(m4.post.fourier))               # from model m2.fourier

df5.post.decomposed
str(df5.post.decomposed)  
# note that this is not a df, so we can't use $ to subset columns. 
# e.g. to get teh season component, we write: 
# df2.decomposed[,'season']

# plot decomposed series: 
autoplot(df5.post.decomposed, facets = TRUE)


# plot all components: 
autoplot(df5.post.decomposed[, 'trend'], 
         series = "Trend") + 
      
      autolayer(df5.post.decomposed[, 'season'], 
                series = "Seasonal") + 
      
      autolayer(df5.post.decomposed[, 'remainder'], 
                series = "Remainder") + 
      
      autolayer(ts2.post.intervention, 
                series = "Raw data") + 
      
      geom_hline(yintercept = 0) + 
      
      
      labs(title = "ED Visits: De-seasonalized trend post-intervention", 
           subtitle = "Jan 2017 to Aug 2018")





# 5) df with pre- and post-intervention de-seasonalized series: -------

df6.trends.pre.and.post <- 
      data.frame(trend.value = c(ts1.pre.intervention - df3.pre.decomposed[, "season"], 
                                 ts2.post.intervention -  df5.post.decomposed[, "season"]),
                 timeperiod = 1:44,
                 post.intervention = c(rep(0, 24), 
                                       rep(1, 20)) %>% as.factor,
                 time.after.intervention = c(rep(0, 24), 
                                             1:20))

df6.trends.pre.and.post


# 5.1) plot pre- and post- trends: 
p7.pre.post.trends <- 
      df6.trends.pre.and.post %>% 
      ggplot(aes(x = timeperiod, 
                 y = trend.value, 
                 group = post.intervention, 
                 colour = post.intervention)) + 
      
      geom_line() + 
      geom_point() + 
      stat_smooth(method = "lm") + 
      
      geom_vline(xintercept = 24,
                 colour = "grey50") + 
      
      geom_vline(xintercept = 25,
                 colour = "grey50") + 
      
      scale_y_continuous(limits = c(0, 20)) + 
      
      theme_minimal(base_size = 14) + 
      theme(panel.border = element_rect(fill = NA)) + 
      
      labs(title = "VGH Purdy Pavilion Evaluation", 
           subtitle = "Acute admits (de-seasonalized) pre- and post- Jan 2017"); p7.pre.post.trends


# 6) segmented regression analysis: ----------

m5.segmented.regression <- lm(trend.value ~ timeperiod + 
                                    post.intervention + 
                                    time.after.intervention, 
                              data = df6.trends.pre.and.post)


summary(m5.segmented.regression)

# > 6.1) diagnostics: -------------
par(mfrow = c(2,2))
plot(m5.segmented.regression)
par(mfrow = c(1,1))

resid(m5.segmented.regression) %>% density() %>% plot

# > 6.2) interpretation of segmented regression: --------
df7.coeff.from.segmented.regression <-  tidy(m5.segmented.regression)

df7.1.predicted.values <- augment(m5.segmented.regression) %>% 
      select(.fitted) %>% 
      rename(fitted.values = .fitted)

df8.coefficient.confints <- confint(m5.segmented.regression,
                                    level = 0.95) 

# remove rownames: 
rownames(df8.coefficient.confints) <- NULL

# final summary of coefficients:
df9.coefficients.with.CIs <- 
      cbind(df7.coeff.from.segmented.regression, 
            df8.coefficient.confints) %>% 
      rename(ci.lower = `2.5 %`, 
             ci.upper = `97.5 %`)


# 6.3) counterfactual estimates (long-term effect of intervention): ----------------

df10.counterfactuals <- 
      df6.trends.pre.and.post %>% 
      
      filter(post.intervention == 1) %>% 
      mutate(predicted.diff.from.counterfactual = 
                   
                   # point estimate off difference from counterfactual: Note:
                   # see p302 of paper "Segmented regression analysis of
                   # interrupted time series studies in medication use research"
                   
                   # coefficient beta2 + beta3 * time after intervention is the
                   # estimate of the long-term effect
                   
                   
                   df9.coefficients.with.CIs$estimate[3] +   
                   df9.coefficients.with.CIs$estimate[4] * time.after.intervention,
            
            
            
            lower.predicted.diff.from.counterfactual = 
      
                   # smallest possible level & trend CHANGES from pre-intervention 
                   df9.coefficients.with.CIs$ci.upper[3] +   
                   df9.coefficients.with.CIs$ci.upper[4] * time.after.intervention, 
             
             
             upper.predicted.diff.from.counterfactual = 
                   
                   # LARGEST possible level & trend CHANGES from pre-intervention 
                   df9.coefficients.with.CIs$ci.lower[3] +   
                   df9.coefficients.with.CIs$ci.lower[4] * time.after.intervention
             
             )


# long-term effect of the intervention over following 20 months: 
df11.long.term.effects <- data.frame(
      estimate.long.term.effect = sum(df10.counterfactuals$predicted.diff.from.counterfactual),   
      lower.long.term.effect = sum(df10.counterfactuals$lower.predicted.diff.from.counterfactual),  
      upper.long.term.effect = sum(df10.counterfactuals$upper.predicted.diff.from.counterfactual)
)

df11.long.term.effects


# > 6.4) MODEL INTERPRETATION: ---------------- 
# pre-intervention y-intercept: 6.8 ED visits 
# pre-intervention slope: +0.37 ED visits per month 

# immediate effect of intervention: change of -9.9 ED visits (95% CI: [-12.9, -6.9]) 

# longer-term effect of intervention: 
# reduction of 293 ED visits over 20 months (95% CI: [-183, -402]) 




# 7) Write outputs: --------------
write_csv(cbind(df6.trends.pre.and.post,
                df7.1.predicted.values),
          here("results",
               "dst",
               "2019-01-31_acute-admits_data-for-segmented-regression-analysis.csv"))
 
 
ggsave(here("results",
            "dst",
            "2019-01-31_acute-admits_data-for-segmented-regression-analysis.pdf"),
       p7.pre.post.trends,
       width = 10)
 
 
write_csv(df9.coefficients.with.CIs,
          here("results",
               "dst",
               "2019-01-31_acute-admits_segmented-regression-model-coefficients.csv"))
 
 
write_csv(df10.counterfactuals,
          here("results",
               "dst",
               "2019-01-31_acute-admits_counterfactual-estimates-for-long-term-effect-of-intervention.csv"))
 
 
