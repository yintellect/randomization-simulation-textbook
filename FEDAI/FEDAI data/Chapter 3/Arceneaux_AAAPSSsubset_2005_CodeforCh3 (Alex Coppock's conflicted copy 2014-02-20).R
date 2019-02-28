# Chapter 3: Clustered Analysis -- example using Kansas City Clustered Design

rm(list=ls())       # clear objects in memory
library(ri)       # load the RI package
library(foreign)    # package allows R to read Stata datasets
set.seed(1234567)   # random number seed, so that results are reproducible

# Data are from Arceneaux, Kevin. 2005. “Using Cluster Randomized Field Experiments to Study Voting Behavior.” The Annals of the American Academy of Political and Social Science 601: 169-79.

# read in data using Stata file - loaded directly from website.
kansas <- read.dta("http://hdl.handle.net/10079/r2280ss")

# Alternatively, you can instead read in data using .csv file
# kansas <- read.csv(file="http://hdl.handle.net/10079/dr7sr5q",head=TRUE,sep=",")

#redefine variables
Z <-  kansas$treatmen
Y <- kansas$vote03
clust <- kansas$unit

covs <- as.matrix(kansas[,2:21])  # covariates are past voter turnout if you care to perform a randomization check

probs <- genprobexact(Z,clustvar=clust)  # subjects are clustered by precinct

numiter <- 5000  # actual number of randomizations in this case is 40116600

perms <- genperms(Z,maxiter=numiter,clustvar=clust)    # clustered assignment
numiter <- ncol(perms)  # reset numiter so that it is no larger than the maximum number of possible randomizations

## show the number of observations
nrow(as.matrix(Y))

## sort the data by cluster size in order to see the range
sort(as.matrix((table(clust))))


## Part (a)
# RI for randomization check
# Use F-test to assess the null hypothesis that the covariates predict random assignment (Z) no better than would be expected by chance

Fstat <- summary(lm(Z~covs))$fstatistic[1]   # F-statistic from actual data

Fstatstore <- rep(NA,numiter)

for (i in 1:numiter) {
	Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]   # F-statistic under the null of random assignment of Z
	}

mean(Fstatstore >= Fstat)

## Part (b) 
# Estimate the ATE using (possibly biased) difference-in-means

ate <- estate(Y,Z,prob=probs)

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate                                  # estimated ATE
sum(distout <= ate)                  # one-tailed comparison
sum(abs(distout) >= abs(ate))        # two-tailed comparison
dispdist(distout,ate)                

# estimate the confidence interval assuming that the true ATE=ate
Ys <- genouts(Y,Z,ate=ate)
distout <- gendist(Ys,perms,prob=probs)
dispdist(distout,ate)      



# Part (c)
# Estimate the ATE using (unbiased but imprecise) difference-in-totals


ateHT <- estate(Y,Z,prob=probs,HT=TRUE)    # Horvitz-Thompson difference-in-totals estimator


distoutHT <- gendist(Ys,perms,prob=probs,HT=TRUE)

ateHT                                # estimated difference-in-totals
sum(distoutHT <= ateHT)             
sum(abs(distoutHT) >= abs(ateHT))

dispdist(distoutHT,ateHT)            # compare to null distribution

