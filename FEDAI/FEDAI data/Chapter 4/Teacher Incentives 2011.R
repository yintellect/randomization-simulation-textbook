
# Analysis of Teacher Incentives experiment (subset of cases with T1 outcomes): 
# Muralidharan, Karthik, and Venkatesh Sundararaman. 2011. 
# “Teacher Performance Pay: Experimental Evidence from India.” 
# Journal of Political Economy 119: 39-77.

rm(list=ls())       # clear objects in memory
library(ri)
set.seed(1234567)   # random number seed, so that results are reproducible

library(foreign)    # package allows R to read Stata datasets


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Possibly interesting examples/Teacher Performance")

teach <- read.dta("Teacher incentives 2011 -- subset.dta")

Z <-  teach$incentive
Y <- teach$y1_nts
clust <- teach$apfschoolcode

covs <- as.matrix(teach[,5:8])  # covariates

probs <- genprobexact(Z,clustvar=clust)  # subjects are clustered by precinct

numiter <- 1000

perms <- genperms(Z,maxiter=numiter,clustvar=clust)    # clustered assignment
numiter <- ncol(perms)  # reset numiter so that it is no larger than the maximum number of possible randomizations

## Part (a)
# RI for the effect of the covariate on the treatment
# Use F-test to assess the null hypothesis that the covariates predict random assignment (Z) no better than would be expected by chance

Fstat <- summary(lm(Z~covs))$fstatistic[1]   # F-statistic from actual data

Fstatstore <- rep(NA,numiter)

for (i in 1:numiter) {
	Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]   # F-statistic under the null of random assignment of Z
	}

mean(Fstatstore >= Fstat)

# Part (b)

ate <- estate(Y,Z,prob=probs)
ate

ate <- estate(Y,Z,X=covs,prob=probs)

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,X=covs,prob=probs)

ate                                  # estimated ATE
sum(distout <= ate)                  # one-tailed comparison
sum(abs(distout) >= abs(ate))        # two-tailed comparison

dispdist(distout,ate)                

# Part (c)

ateHT <- estate(Y,Z,prob=probs,HT=TRUE)    # Horvitz-Thompson difference-in-totals estimator

ateHT

# Part (d)

distoutHT <- gendist(Ys,perms,prob=probs,HT=TRUE)

ateHT                                # estimated difference-in-totals
sum(distoutHT <= ateHT)             
sum(abs(distoutHT) >= abs(ateHT))

dispdist(distoutHT,ateHT)            # compare to null distribution

# Part (e)

Ypre <- teach$pretest              # use pretest scores to create difference scores
Ypre <- Ypre - 99*teach$pretest_miss

ateHT2 <- estate(Y,Z,Ypre=Ypre,prob=probs,HT=TRUE)  # difference-in-differenced totals

# Part (f)

distoutHT2 <- gendist(Ys,perms,Ypre=Ypre,prob=probs,HT=TRUE)

ateHT2
sum(distoutHT2 <= ateHT2)
sum(abs(distoutHT2) >= abs(ateHT2))

dispdist(distoutHT2,ateHT2)