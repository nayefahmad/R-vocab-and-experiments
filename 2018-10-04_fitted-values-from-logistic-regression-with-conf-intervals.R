

#***********************************************
# FITTED VALUES FROM LOGISTIC REGRESSION WITH CIs 
#***********************************************

library(ISLR)
library(tidyverse)
library(magrittr)

# reference: https://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

set.seed(1234)

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


