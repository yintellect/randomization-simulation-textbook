# Exercise 4.10 -- Kansas City Clustered Design

rm(list=ls())       # clear objects in memory
setwd("/Users/donaldgreen/Desktop/RI/RI package (beta version)")
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Datasets for Website/")

# Data are from Arceneaux, Kevin. 2005. 
# “Using Cluster Randomized Field Experiments to Study Voting Behavior.” 
# The Annals of the American Academy of Political and Social Science 601: 169-79.
kansas <- read.dta("Problem 4.10 Arceneaux.dta")

#### Problem 4.10a ####
Z <-  kansas$treatmen
Y <- kansas$vote03
clust <- kansas$unit

# covariates are past voter turnout
covs <- as.matrix(kansas[,2:21])  

# subjects are clustered by precinct
probs <- genprobexact(Z,clustvar=clust)  

# clustered assignment
perms <- genperms(Z,maxiter=1000,clustvar=clust)    
numiter <- ncol(perms)

# F-statistic from actual data
Fstat <- summary(lm(Z~covs))$fstatistic[1]   

# Run simulation
Fstatstore <- rep(NA,numiter)
for (i in 1:numiter) {
  Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]   
}

p.value <- mean(Fstatstore >= Fstat)
p.value

#### Problem 4.10b ####
ate <- estate(Y,Z,X=covs,prob=probs)
Ys <- genouts(Y,Z,ate=0)
distout <- gendist(Ys,perms,X=covs,prob=probs)
p.value.onetailed <- mean(distout >= ate)

# estimated ATE
ate
# one-tailed comparison
p.value.onetailed


#### Problem 4.10c ####

# Horvitz-Thompson difference-in-totals estimator
ateHT <- estate(Y,Z,prob=probs,HT=TRUE) 
# estimated difference-in-totals
ateHT

#### Problem 4.10d ####

distoutHT <- gendist(Ys,perms,prob=probs,HT=TRUE)
p.value.onesidedHT <- mean(distoutHT >= ateHT)
p.value.onesidedHT


#### Problem 4.10e ####

# use average turnout in past elections as a covariate 
# that will be considered a "pretest" and subtracted from Y
Y_diff <- Y - rowMeans(covs) 

# difference-in-differenced totals
ateHT2 <- estate(Y_diff,Z,prob=probs,HT=TRUE)  
Ys <- genouts(Y_diff,Z,ate=0)
distoutHT2 <- gendist(Ys,perms,prob=probs,HT=TRUE)
p.value.onesidedHT2 <- mean(distoutHT2 >= ateHT2)

# estimated difference-in-differenced totals
ateHT2
p.value.onesidedHT2

