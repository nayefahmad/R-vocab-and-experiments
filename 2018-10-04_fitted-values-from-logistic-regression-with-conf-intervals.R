

#***********************************************
# FITTED VALUES FROM LOGISTIC REGRESSION WITH CIs 
#***********************************************

library(ISLR)
library(tidyverse)
library(magrittr)

# reference: https://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

set.seed(1234)

# FAKE DATA: -----------
# create fake data on gambling. Does prob win depend on bid size? 
mydat <- data.frame(
      won=as.factor(sample(c(0, 1), 250, replace=TRUE)), 
      bid=runif(250, min=0, max=1000)
)

# logistic regression model: 
mod1 <- glm(won~bid, data=mydat, family=binomial(link="logit"))

# new predictor values to use for prediction: 
plotdat <- data.frame(bid=(0:1000))

# df with predictions, lower and upper limits of CIs: 
preddat <- predict(mod1,
                   type = "link",
                   newdata=plotdat,
                   se.fit=TRUE) %>% 
      as.data.frame() %>% 
      mutate(bid = (0:1000), 
             
             # model object mod1 has a component called linkinv that 
             # is a function that inverts the link function of the GLM:
             lower = mod1$family$linkinv(fit - 1.96*se.fit), 
             point.estimate = mod1$family$linkinv(fit), 
             upper = mod1$family$linkinv(fit + 1.96*se.fit)) 


# plotting with ggplot: 
preddat %>% ggplot(aes(x = bid, 
                       y = point.estimate)) + 
      geom_line(colour = "blue") + 
      geom_ribbon(aes(ymin = lower,
                      ymax = upper), 
                  alpha = 0.5) + 
      scale_y_continuous(limits = c(0,1))




# LET'S TRY WITH ANOTHER DATASET: ---------
df1.default <- ISLR::Default

# fit model: 
m2 <- glm(default ~ student + balance + income, 
          data = df1.default,
          family = binomial(link = "logit"))

# let's examine relationship between default for students and non-students, 
# holding income at it's median values: 

df2.medians <- df1.default %>% 
      group_by(student) %>% 
      summarise(median.inc = median(income), 
                max.inc = max(income), 
                max.bal = max(balance)) %>% print

# dataset for students: 
df3.student.newdata <- data.frame(default = rep(NA, df2.medians$max.bal[2]) %>% 
                                        factor(levels = c("No", "Yes")), 
                                  student = rep("No", df2.medians$max.bal[2]) %>% 
                                        factor(levels = c("No", "Yes")), 
                                  balance = 1:df2.medians$max.bal[2], 
                                  income = rep(df2.medians$median.inc[2], 
                                               df2.medians$max.bal[2]))

# predictions for students: 
df4.student.preddat <- predict(m2,
                   type = "link",
                   newdata=df3.student.newdata,
                   se.fit=TRUE) %>% 
      as.data.frame() %>% 
      mutate(balance = 1:df2.medians$max.bal[2],
            
             # model object mod1 has a component called linkinv that 
             # is a function that inverts the link function of the GLM:
             lower = m2$family$linkinv(fit - 1.96*se.fit), 
             point.estimate = m2$family$linkinv(fit), 
             upper = m2$family$linkinv(fit + 1.96*se.fit)) 

tail(df4.student.preddat)


# plotting with ggplot: 
p1.students <- df4.student.preddat %>% 
      ggplot(aes(x = balance, 
                 y = point.estimate)) + 
      geom_line(colour = "blue") + 
      geom_ribbon(aes(ymin = lower,
                      ymax = upper), 
                  alpha = 0.5) + 
      scale_y_continuous(limits = c(0,1)); p1.students
