# Exercise 4.5 (restricted randomization)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")

#  Data are from Table 4.1
teach <- read.dta("Problem 4.4 Teachers.dta")

#### Problem 4.5a ####
D  <- teach$D
Y1 <- teach$y1
Y0 <- teach$y0
X <- teach$x

Y <- Y0*(1-D) + Y1*(D)
N <- length(D)

# Write a restricted randomization function
randfun <- function() {
  teststat <- -1
  while (teststat < 0.05) {
    Zri <- sample(D)
    teststat <- summary(lm(Zri~X))$coefficients[2,4]
  }
  return(Zri)
}

# notice the use of the restricted randomization function.
# restricted randomization often generates unequal probabilities of assignment. 
# if so, inverse probability weighting is required.

perms <- genperms.custom(numiter=10000,randfun=randfun)
probs <- genprob(perms)
weights <- (1/probs) *D + (1/(1-probs))*(1-D)

# Calculate the variance of the weights
var.weights.treat <- var(weights[D==1])
var.weights.control <- var(weights[D==0])

#### Problem 4.5b ####

ate <- estate(Y,D,prob=probs)    
Ys <- genouts(Y,D,ate=0)
distout <- gendist(Ys,perms,prob=probs)
p.value <- mean(abs(distout) > abs(ate)) 
ate
p.value

#### Problem 4.5c ####

perms <- genperms.custom(numiter=10000,randfun=randfun)
probs <- genprob(perms)
ate_cov <- estate(Y,D,X,prob=probs)    
Ys <- genouts(Y,D,ate=0)
distout_cov <- gendist(Ys,perms,X,prob=probs)
p.value_cov <- mean(abs(distout_cov) > abs(ate_cov)) 
ate_cov
p.value_cov

#### Problem 4.5d ####

perms_complete_RA <- genperms(D,maxiter=10000)
probs_complete_RA <- genprobexact(D)

ate_complete_RA <- estate(Y,D,prob=probs_complete_RA)
Ys_complete_RA <- genouts(Y,D,ate=ate_complete_RA)  
distout_complete_RA <- gendist(Ys_complete_RA,perms_complete_RA,
                               prob=probs_complete_RA)
se_complete_RA <- sd(distout_complete_RA)
se_complete_RA

ate_cov_complete_RA <- estate(Y,D,X,prob=probs_complete_RA)
Ys_cov_complete_RA <- genouts(Y,D,ate=ate_cov_complete_RA)  
distout_cov_complete_RA <- gendist(Ys_cov_complete_RA,perms_complete_RA,X,
                                   prob=probs_complete_RA)
se_cov_complete_RA <- sd(distout_cov_complete_RA)
se_cov_complete_RA

# Sampling Distributions from 5(a) and 5(b)
perms_restricted_RA <- genperms.custom(numiter=10000,randfun=randfun)
probs_restricted_RA <- genprob(perms_restricted_RA)

ate_restricted_RA <- estate(Y,D,prob=probs_restricted_RA)
Ys_restricted_RA <- genouts(Y,D,ate=ate_restricted_RA)  
distout_restricted_RA <- gendist(Ys_restricted_RA,perms_restricted_RA,
                                 prob=probs_restricted_RA)
se_restricted_RA <- sd(distout_restricted_RA)
se_restricted_RA

ate_cov_restricted_RA <- estate(Y,D,X,prob=probs_restricted_RA)
Ys_cov_restricted_RA <- genouts(Y,D,ate=ate_cov_restricted_RA)  
distout_cov_restricted_RA <- gendist(Ys_cov_restricted_RA,perms_restricted_RA,X,
                                     prob=probs_restricted_RA)
se_cov_restricted_RA <- sd(distout_cov_restricted_RA)
se_cov_restricted_RA