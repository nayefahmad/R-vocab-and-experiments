

#***********************************************
# FITTED VALUES FROM LOGISTIC REGRESSION WITH CIs 
#***********************************************

# reference: https://stats.stackexchange.com/questions/29044/plotting-confidence-intervals-for-the-predicted-probabilities-from-a-logistic-re

set.seed(1234)
mydat <- data.frame(
      won=as.factor(sample(c(0, 1), 250, replace=TRUE)), 
      bid=runif(250, min=0, max=1000)
)


mod1 <- glm(won~bid, data=mydat, family=binomial(link="logit"))
plotdat <- data.frame(bid=(0:1000))
preddat <- predict(mod1, newdata=plotdat, se.fit=TRUE)
with(mydat, plot(bid, won, type="n", 
                 ylim=c(0, 1), ylab="Probability of winning", xlab="Bid"))
with(preddat, lines(0:1000, exp(fit)/(1+exp(fit)), col="blue"))
with(preddat, lines(0:1000, exp(fit+1.96*se.fit)/(1+exp(fit+1.96*se.fit)), lty=2))
with(preddat, lines(0:1000, exp(fit-1.96*se.fit)/(1+exp(fit-1.96*se.fit)), lty=2))