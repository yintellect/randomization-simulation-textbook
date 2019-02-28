# Exercise 4.10 -- Kansas City Clustered Design

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
setwd("~/Desktop/2016/4368 Experimental/5.FEDAI replication/Chapter 4")



library(foreign)    # package allows R to read Stata datasets

# Data are from Arceneaux, Kevin. 2005. “Using Cluster Randomized Field Experiments to Study Voting Behavior.” The Annals of the American Academy of Political and Social Science 601: 169-79.

kansas <- read.dta("Chapter 4_Arceneaux (2005) Dataset.dta")

Z <- kansas$treatmen
Y <- kansas$vote03
clust <- kansas$unit
covs <- as.matrix(kansas[,2:21]) # covariates are past voter turnout
probs <- genprobexact(Z,clustvar=clust) # subjects are clustered by precinct 
perms <- genperms(Z,maxiter=1000,clustvar=clust) # clustered assignment
## Too many permutations to use exact method.
## Defaulting to approximate method.
## Increase maxiter to at least 40116600 to perform exact estimation.
numiter <- ncol(perms)
Fstat <- summary(lm(Z~covs))$fstatistic[1] # F-statistic from actual data
Fstatstore <- rep(NA,numiter)
for (i in 1:numiter) {
        Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]
}
p.value <- mean(Fstatstore >= Fstat)
p.value

ate <- estate(Y,Z,X=covs,prob=probs)
Ys <- genouts(Y,Z,ate=0)
distout <- gendist(Ys,perms,X=covs,prob=probs)
p.value.onetailed <- mean(distout >= ate)

# Part (b)

ate <- estate(Y,Z,X=covs,prob=probs)

Ys <- genouts(Y,Z,ate=ate)

distout <- gendist(Ys,perms,X=covs,prob=probs)

ate                                  # estimated ATE
sum(distout <= ate)                  # one-tailed comparison
sum(abs(distout) >= abs(ate))        # two-tailed comparison
p.value.onetailed <- mean(distout >= ate)
dispdist(distout,ate)                

# Part (c)

ateHT <- estate(Y,Z,prob=probs,HT=TRUE)    # Horvitz-Thompson difference-in-totals estimator

# Part (d)

distoutHT <- gendist(Ys,perms,prob=probs,HT=TRUE)

ateHT                                # estimated difference-in-totals
sum(distoutHT <= ateHT)             
sum(abs(distoutHT) >= abs(ateHT))

dispdist(distoutHT,ateHT)            # compare to null distribution

# Part (e)

Ypre <- rowMeans(covs)               # use average turnout in past elections as a covariate that will be considered a "pretest" and subtracted from Y

ateHT2 <- estate(Y,Z,Ypre=Ypre,prob=probs,HT=TRUE)  # difference-in-differenced totals

# Part (f)
Ys <- genouts(Y,Z,ate=ateHT2)
distoutHT2 <- gendist(Ys,perms,Ypre=Ypre,prob=probs,HT=TRUE)

ateHT2
sum(distoutHT2 <= ateHT2)
sum(abs(distoutHT2) >= abs(ateHT2))

dispdist(distoutHT2,ateHT2)
