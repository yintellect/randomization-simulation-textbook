# Exercise 3.9 parts b and e

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
setwd("~/MA/2019Spring/RAship/FEDAI/FEDAI data/Chapter 3")
library(foreign)    # package allows R to read Stata datasets

# Data are from Camerer, Colin F. . 1998. “Can Asset Markets Be Manipulated? A Field Experiment with Racetrack Betting.” Journal of Political Economy 106(3): 457-482.

camerer <- read.dta("Camerer data for Chapter 3 exercises.dta")
colnames(camerer)

Z <-  camerer$treatment
Y <- camerer$experimentbets
block <- camerer$pair        # indicates how the subjects are blocked      

# RI for the effect of the covariate on the treatment

covs <- as.matrix(camerer$preexperimentbets)     # reads in covariates

probs <- genprobexact(Z,blockvar=block)

numiter <- 1000

perms <- genperms(Z,maxiter=numiter,blockvar=block)
numiter <- ncol(perms)  # reset numiter so that it is no larger than the maximum number of possible randomizations

## Use F-test to assess the null hypothesis that the covariates predict random assignment (Z) no better than would be expected by chance

Fstat <- summary(lm(Z~covs))$fstatistic[1]   # F-statistic from actual data

Fstatstore <- rep(NA,numiter)

for (i in 1:numiter) {
	Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]  # F-statistic under the null of random assignment of Z
	}

mean(Fstatstore >= Fstat)                     # p-value



# RI for the effect of the treatment on the outcome


probs <- genprobexact(Z,blockvar=block)    # notice the use of block to indicate how the random assignment was conducted

ate <- estate(Y,Z,prob=probs)

perms <- genperms(Z,maxiter=10000,blockvar=block)    # simulated block random assignments

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate
sum(distout <= ate)
sum(abs(distout) >= abs(ate))

dispdist(distout,ate)   # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

change <- camerer$experimentbets
change_treatment <- mean(change[Z==1])
change_control <- mean(change[Z==0])
ATE <- change_treatment - change_control
ATE
