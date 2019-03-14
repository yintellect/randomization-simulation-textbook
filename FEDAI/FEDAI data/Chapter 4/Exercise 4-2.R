#   -----------------------------------------------------------------------
#    Exercise 4.2 (Game-playing experiment)
#   -----------------------------------------------------------------------
rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
setwd("~/MA/2019Spring/RAship/FEDAI/FEDAI data/Chapter 4")
rush <- read.dta("RushHour data for exercise 4-2.dta")

#   -----------------------------------------------------------------------
#    (a)
#   -----------------------------------------------------------------------
Z <- rush$treat       # treatment is thinking strategies curriculum
Y <- rush$posttest    # number of puzzled solved during the testing session
X <- rush$pretest

# RI: randomization check, testing the effect of the covariate on the treatment

covs <- as.matrix(rush$pretest)     # covariate is the pretest score

probs <- genprobexact(Z)

numiter <- 50000   # set the number of simulated random assignments (if you set it to 48620 or higher, your results will be exact because that is the true number of possible random assignments)

perms <- genperms(Z,maxiter=numiter)  
numiter <- ncol(perms)  # reset numiter so that it is no larger than the maximum number of possible randomizations

Fstat <- summary(lm(Z~covs))$fstatistic[1]  # observed F statistic
Fstat

Fstatstore <- rep(NA,numiter)    # initialize vector of simulated F statistics

for (i in 1:numiter) {
	Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]
	}
sum(Fstatstore >= Fstat)
mean(Fstatstore >= Fstat)    # calculate p-value

#   -----------------------------------------------------------------------
#    (b)
#   -----------------------------------------------------------------------

# look at ATE using raw scores as outcomes (ignoring pretest)

perms <- genperms(Z,maxiter=numiter)

probs <- genprobexact(Z)

ate <- estate(Y,Z,prob=probs)
ate
Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate                         # estimated ATE
dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null



# compute confidence intervals
Ys <- genouts(Y,Z,ate=ate)  # the statement ate=ate imposes the assumption that the true treatment effect for every unit is equal to the estimated ATE

distout <- gendist(Ys,perms,prob=probs)

ate
dispdist(distout,ate)   # display p-values (ignore these), 95% confidence interval, standard error under the assumed DGP, and graph the sampling distribution under the assumption that the Y1-Y0=estimated(ATE) for all units

#   -----------------------------------------------------------------------
#    (c)
#   -----------------------------------------------------------------------

# look at ATE using improvement scores (posttest-pretest) as outcomes

Y <- rush$improvement
perms <- genperms(Z,maxiter=numiter)
probs <- genprobexact(Z)
ate <- estate(Y,Z,prob=probs)
Ys <- genouts(Y,Z,ate=0)   # the statement ate=0 imposes the assumption that the true treatment effect for every unit is 0
distout <- gendist(Ys,perms,prob=probs)

ate
dispdist(distout,ate)


# compute confidence intervals
Ys <- genouts(Y,Z,ate=ate)   # the statement ate=ate imposes the assumption that the true treatment effect for every unit is equal to the estimated ATE
distout <- gendist(Ys,perms,prob=probs)
ate
dispdist(distout,ate)



# for fun, look at effect on posttest controlling for covariate using regression and compare RI results to regression output
Y <- rush$posttest
perms <- genperms(Z,maxiter=numiter)
probs <- genprobexact(Z)
ateX <- estate(Y,Z,X,prob=probs)  # this syntax controls for X
Ys <- genouts(Y,Z,ate=0)
distoutX <- gendist(Ys,perms,X,prob=probs)
ateX
dispdist(distoutX,ateX)

summary(lm(Y~Z+X))  # the estimated ATE is identical, compare p-values and SEs



