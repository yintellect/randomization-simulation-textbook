# Exercise 4.4 (teachers experiment)
rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
setwd("~/Desktop/2016/4368 Experimental/5.FEDAI replication/Chapter 4")

#  Data are from Table 4.1

teach <- read.dta("Teachers data for Table 4-1.dta")
colnames(teach)

Z  <- teach$D       # treatment
Y1 <- teach$y1      # treated potential outcome
Y0 <- teach$y0      # untreated potential outcome
X <- teach$x        # pre-test

Y <- Y0*(1-Z) + Y1*(Z)    # observed outcomem given random assignment
N <- length(Z)

# Part (a)

# two equivalent ways to estimate the ATE: regression and difference-in-means
summary(lm(Y~Z))
mean(Y[Z==1])-mean(Y[Z==0])

# Part (b)

# a diagnostic test to assess whether rescaling the outcome as a change
# rather than a level is likely to improve the precision 
# with which the ATE is estimated (see equation 4.6)
lm(Y~X,subset=Z==1)
lm(Y~X,subset=Z==0)

Ydiff <- Y-X
summary(lm(Ydiff~Z))

# Part (c)

summary(lm(Y~Z+X))   # estimate the ATE using covariate adjustment

# Part (d)

perms <- genperms(Z,maxiter=10)  # simulate possible random allocations
probs <- genprobexact(Z)
ate <- estate(Y,Z,prob=probs)       # estimate the ATE
Ys <- genouts(Y,Z,ate=0)            # calculate potential outcomes under sharp null of 0 effect for all units

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate                                 # report the estimated ATE
dispdist(distout,ate)               # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null
sum(distout>=ate)
# Part (e)

ateX <- estate(Y,Z,X,prob=probs)   # estimate the ATE using covariate adjustment
distoutX <- gendist(Ys,perms,X,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis
ateX                       # report the estimated ATE
dispdist(distoutX,ateX)    


# Part (f)

Ys2 <- genouts(Y,Z,ate=ate)   # generate potential outcomes assuming that the true treatment effect for every unit is equal to the estimated ATE
distout2 <- gendist(Ys2,perms,prob=probs)

ate
dispdist(distout2,ate)        # display the sampling distribution under the assumed treatment effect in order to obtain the confidence interval
abline(v=1.83000,lty=3,col="red",lwd=3)
abline(v=19.63025 ,lty=3,col="red",lwd=3)
# Part (g)

YsX <- genouts(Y,Z,ate=ateX)  # generate potential outcomes assuming that the true treatment effect for every unit is equal to the estimated ATE under covariate adjustment
distoutX2 <- gendist(YsX,perms,X,prob=probs)

ateX
dispdist(distoutX2,ateX)
abline(v=2.278792,lty=3,col="red",lwd=3)
abline(v=8.403560,lty=3,col="red",lwd=3)
