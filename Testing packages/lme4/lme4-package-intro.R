##-------------------------------------------------------------------------
## ----preliminaries-------------------------------------------------------
library("lme4")
library("lattice")
library("minqa")

##-------------------------------------------------------------------------
## 1. Introduction
## 1.2. Example
str(sleepstudy)

xyplot(Reaction ~ Days | Subject, sleepstudy, aspect = "xy",
       layout = c(9, 2), type = c("g", "p", "r"),
       index.cond = function(x, y) coef(lm(y ~ x))[2],
       xlab = "Days of sleep deprivation",
       ylab = "Average reaction time (ms)",
       as.table = TRUE)

fm1 <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)


## 1.3. High level modular structure
parsedFormula <- lFormula(formula = Reaction ~ Days + (Days | Subject),
                          data = sleepstudy)
devianceFunction <- do.call(mkLmerDevfun, parsedFormula)
optimizerOutput <- optimizeLmer(devianceFunction)
mkMerMod(rho = environment(devianceFunction),
         opt = optimizerOutput,
         reTrms = parsedFormula$reTrms,
         fr = parsedFormula$fr)

##-------------------------------------------------------------------------
## 2. Formula module
## 2.2: Understanding mixed-model formulas
fm2 <- lmer(Reaction ~ Days + (Days || Subject), sleepstudy)


## 2.3. Algebraic and computational account of mixed-model formulas
set.seed(2)
## ----factorToSparseMatrix------------------------------------------------
(f <- gl(3, 2))
(Ji <- t(as(f, Class = "sparseMatrix")))
## ----rawModelMatrix------------------------------------------------------
(Xi <- cbind(1, rep.int(c(-1, 1), 3L)))
## ----KhatriRao-----------------------------------------------------------
(Zi <- t(KhatriRao(t(Ji), t(Xi))))
## alternative formulation of Zi (eq:Zi)
rBind(
  Ji[1,] %x% Xi[1,],
  Ji[2,] %x% Xi[2,],
  Ji[3,] %x% Xi[3,],
  Ji[4,] %x% Xi[4,],
  Ji[5,] %x% Xi[5,],
  Ji[6,] %x% Xi[6,])
## ----template------------------------------------------------------------
nc <- 3
(rowIndices <- rep(1:nc, 1:nc))
(colIndices <- sequence(1:nc))
(template <- sparseMatrix(rowIndices, colIndices,
  x = 1 * (rowIndices == colIndices)))
## ----thetaFromTemplate---------------------------------------------------
(theta <- template@x)
## ----thetaSanityCheck----------------------------------------------------
lFormula(Reaction ~ (Days + I(Days^2) | Subject), sleepstudy)$reTrms$theta
## ----relativeCovarianceBlock---------------------------------------------
nl <- 2
(Lambdati <- .bdiag(rep(list(t(template)), nl)))
## ----relativeCovarianceBlockIndices--------------------------------------
LindTemplate <- rowIndices + nc * (colIndices - 1) - choose(colIndices, 2)
(Lind <- rep(LindTemplate, nl))
## ----newTheta------------------------------------------------------------
thetanew <- round(runif(length(theta)), 1)
## ----relativeCovarianceBlockUpdate---------------------------------------
Lambdati@x <- thetanew[Lind]

##-------------------------------------------------------------------------
## 3. Objective function module
## 3.6. Penalized least squares algorithm
## ----PLSupdateThetaWithLind----------------------------------------------
mapping <- function(theta) theta[Lind]
## ----exampleNewTheta-----------------------------------------------------
thetanew <- c(1, -0.1, 2, 0.1, -0.2, 3)
## ----exampleLindUpdate---------------------------------------------------
Lambdati@x[] <- mapping(thetanew)
Lambdati

##-------------------------------------------------------------------------
## 5. Output module
## 5.1. Theory underlying the output module
## ----vcovByHand----------------------------------------------------------
RX <- getME(fm1, "RX")
sigma2 <- sigma(fm1)^2
sigma2 * chol2inv(RX)

## 5.2. Using the output module
## ----updateModel---------------------------------------------------------
fm3 <- update(fm1, . ~ . - (Days | Subject) + (1 | Subject))
formula(fm3)

## ----summary-------------------------------------------------------------
ss <- summary(fm1)
cc <- capture.output(print(ss))
reRow <- grep("^Random effects", cc)
cat(cc[1:(reRow - 2)], sep = "\n")
## ----summary-reproduced--------------------------------------------------
formula(fm1)
REMLcrit(fm1)
quantile(residuals(fm1, "pearson", scaled = TRUE))

## ----summary-------------------------------------------------------------
feRow <- grep("^Fixed effects", cc)
cat(cc[reRow:(feRow - 2)], sep = "\n")
## ----summary-reproduced--------------------------------------------------
print(vc <- VarCorr(fm1), comp = c("Variance", "Std.Dev."))
nobs(fm1)
ngrps(fm1)
sigma(fm1)

## ----VarCorr-------------------------------------------------------------
as.data.frame(VarCorr(fm1))

## ----summary3------------------------------------------------------------
corRow <- grep("^Correlation", cc)
cat(cc[feRow:(corRow - 2)], sep = "\n")

## ----summary4------------------------------------------------------------
cat(cc[corRow:length(cc)], sep = "\n")

## ----diagplot1-----------------------------------------------------------
plot(fm1, type = c("p", "smooth"))

## ----diagplot2-----------------------------------------------------------
plot(fm1, sqrt(abs(resid(.))) ~ fitted(.),
     type = c("p", "smooth"))

## ----diagplot3-----------------------------------------------------------
qqmath(fm1, id = 0.05)

## ----ppsim---------------------------------------------------------------
iqrvec <- sapply(simulate(fm1, 1000), IQR)
obsval <- IQR(sleepstudy$Reaction)
post.pred.p <- mean(obsval >= c(obsval, iqrvec))

## ----anovaQuadraticModel-------------------------------------------------
fm4 <- lmer(Reaction ~ polyDays[ , 1] + polyDays[ , 2] +
  (polyDays[ , 1] + polyDays[ , 2] | Subject),
  within(sleepstudy, polyDays <- poly(Days, 2)))
anova(fm4)

## ----anovaSanityCheck----------------------------------------------------
(getME(fm4, "RX")[2, ] %*% getME(fm4, "fixef"))^2

## ----anovaManyModels-----------------------------------------------------
anova(fm1, fm2, fm3)

## ----anovaRes,echo=FALSE-------------------------------------------------
fm3ML <- refitML(fm3)
fm2ML <- refitML(fm2)
fm1ML <- refitML(fm1)
ddiff <- deviance(fm3ML) - deviance(fm2ML)
dp <- pchisq(ddiff, 1, lower.tail = FALSE)
ddiff2 <- deviance(fm2ML) - deviance(fm1ML)

## ----pvaluesHelp---------------------------------------------------------
help("pvalues")

## ----compareCI-----------------------------------------------------------
ccw <- confint(fm1, method = "Wald")
ccp <- confint(fm1, method = "profile", oldNames = FALSE)
ccb <- confint(fm1, method = "boot")

## ----CIqcomp-------------------------------------------------------------
rtol <- function(x,y) {
    abs((x - y) / ((x + y) / 2))
}
bw <- apply(ccb, 1, diff)
pw <- apply(ccp, 1, diff)
mdiffpct <- round(100 * max(rtol(bw, pw)))

## ----profile_calc--------------------------------------------------------
pf <- profile(fm1)
xyplot(pf)
densityplot(pf)
splom(pf)

##-------------------------------------------------------------------------
## A. Modularization examples
## A.1. Zero slope-slope covariance models
## ----modularSimulationFormula--------------------------------------------
form <- respVar ~ 1 + (explVar1 + explVar2 | groupFac)

## ----dataTemplate--------------------------------------------------------
set.seed(1)
dat <- mkDataTemplate(form,
  nGrps = 500,
  nPerGrp = 20,
  rfunc = rnorm)
head(dat)

## ----parsTemplate--------------------------------------------------------
(pars <- mkParsTemplate(form, dat))

## ----simCorr-------------------------------------------------------------
vc <- matrix(c(1.0, 0.5, 0.5,
  0.5, 1.0, 0.0,
  0.5, 0.0, 1.0), 3, 3)

## ----vcTheta-------------------------------------------------------------
pars$theta[] <- Vv_to_Cv(mlist2vec(vc))

## ----varCorrStructure----------------------------------------------------
dat$respVar <- simulate(form,
  newdata = dat,
  newparams = pars,
  family = "gaussian")[[1]]

## ----graphSims-----------------------------------------------------------
formLm <- form
formLm[[3]] <- findbars(form)[[1]]
print(formLm)
cor(t(sapply(lmList(formLm, dat), coef)))

## ----phiToTheta----------------------------------------------------------
phiToTheta <- function(phi) {
    theta5 <- -(phi[2]*phi[3])/phi[4]
    c(phi[1:4], theta5, phi[5])
}

## ----compute deviance function modular-----------------------------------
lf <- lFormula(formula = form, data = dat, REML = TRUE)
devf <- do.call(mkLmerDevfun, lf)

## ----wrapper modular-----------------------------------------------------
devfWrap <- function(phi) devf(phiToTheta(phi))

## ----opt modular---------------------------------------------------------
opt <- bobyqa(par = lf$reTrms$theta[-5],
  fn = devfWrap,
  lower = lf$reTrms$lower[-5])

## ----varCorr modular-----------------------------------------------------
vcEst <- vec2mlist(Cv_to_Vv(phiToTheta(opt$par)))[[1]]
dimnames(vcEst) <- rep(lf$reTrms$cnms, 2)
round(vcEst, 2)
vc

## A.2. Additive models
## ----simulateSplineData--------------------------------------------------
library("gamm4")
library("splines")
set.seed(1)
n <- 100
pSimulation <- 3
pStatistical <- 8
x <- rnorm(n)
Bsimulation <- ns(x, pSimulation)
Bstatistical <- ns(x, pStatistical)
beta <- rnorm(pSimulation)
y <- as.numeric(Bsimulation %*% beta + rnorm(n, sd = 0.3))

## ----splineExampleDataPlot-----------------------------------------------
par(mar = c(4, 4, 1, 1), las = 1, bty = "l")
plot(x, y, las = 1)
lines(x[order(x)], (Bsimulation %*% beta)[order(x)])


## ----splineExampleApproximateFormula-------------------------------------
pseudoGroups <- as.factor(rep(1:pStatistical, length = n))
parsedFormula <- lFormula(y ~ x + (1 | pseudoGroups))


## ----splineExampleModifyZt-----------------------------------------------
parsedFormula$reTrms <- within(parsedFormula$reTrms, {
    Bt <- t(as.matrix(Bstatistical))[]
    cnms$pseudoGroups <- "spline"
    Zt <- as(Bt, class(Zt))
})


## ----splineExampleRemainingModularSteps----------------------------------
devianceFunction <- do.call(mkLmerDevfun, parsedFormula)
optimizerOutput <- optimizeLmer(devianceFunction)
mSpline <- mkMerMod(rho = environment(devianceFunction),
                    opt = optimizerOutput,
                    reTrms = parsedFormula$reTrms,
                    fr = parsedFormula$fr)
mSpline


## ----splineExampleFittedModelPlot----------------------------------------
xNew <- seq(min(x), max(x), length = 100)
newBstatistical <- predict(Bstatistical, xNew)
yHat <-   cbind(1, xNew) %*% getME(mSpline, "fixef") +
  newBstatistical %*% getME(mSpline, "u")
par(mar = c(4, 4, 1, 1), las = 1, bty = "l")
plot(x, y)
lines(xNew, yHat)
lines(x[order(x)], (Bsimulation %*% beta)[order(x)], lty = 2)
legend("topright", bty = "n", c("fitted", "generating"), lty = 1:2, col = 1)

