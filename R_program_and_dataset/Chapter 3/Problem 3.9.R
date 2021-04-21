# Exercise 3.9 

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3/")

library(foreign)    # package allows R to read Stata datasets

# Data are from Camerer, Colin F. . 1998. 
# “Can Asset Markets Be Manipulated? A Field Experiment with Racetrack Betting.” 
# Journal of Political Economy 106(3): 457-482.

camerer <- read.dta("Problem 3.9 Camerer.dta")

#### Problem 3.9b ####

D <-  camerer$treatment
# indicates how the subjects are blocked      
block <- camerer$pair 
# reads in covariates
covs <- as.matrix(camerer$preexperimentbets) 

# Generate probabilites of assignment
probs <- genprobexact(D,blockvar=block)

# Generate permuation matrix
perms <- genperms(D,maxiter=10000,blockvar=block)

# reset numiter so that it is no larger than the 
# maximum number of possible randomizations
numiter <- ncol(perms)

# Use F-test to assess the null hypothesis that the covariates 
# predict random assignment (Z) no better than would be expected by chance
Fstat <- summary(lm(D~covs))$fstatistic[1]
Fstatstore <- rep(NA,numiter)

for (i in 1:numiter) {
  # F-statistic under the null of random assignment of Z
  Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]
}

p.value <- mean(Fstatstore >= Fstat)
p.value

#### Problem 3.9c ####

change <- camerer$change
change_treatment <- mean(change[D==1])
change_control <- mean(change[D==0])
ATE <- change_treatment - change_control
ATE

#### Problem 3.9d ####

pair_diffs <- rep(NA, 17)

for (i in 1:17){
  pair_diffs[i] <- diff(change[block==i])
}

mean(pair_diffs)

#### Problem 3.9e ####

set.seed(1234567)
# Notice the blocks
probs <- genprobexact(D,blockvar=block) 
ate <- estate(change,D,prob=probs)
perms <- genperms(D,maxiter=10000,blockvar=block)
Ys <- genouts(change,D,ate=0)
distout <- gendist(Ys,perms,prob=probs) 

ate
p.value <- mean(abs(distout) >= abs(ate))
p.value
