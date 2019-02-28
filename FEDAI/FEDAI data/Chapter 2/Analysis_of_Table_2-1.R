# analysis of Table 2.1

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 2/")

library(foreign)    # package allows R to read Stata datasets

# Data are from Table 2-1

villages <- read.dta("Table 2-1.dta")
colnames(villages)

Z <-  c(1,0,0,0,0,0,1)       # one possible treatment assignment
Y <-  villages$Y1*Z + villages$Y0*(1-Z)  # observed outcomes given assignment


probs <- genprobexact(Z,blockvar=NULL)   # no blocking is assumed when generating probability of treatment

ate <- estate(Y,Z,prob=probs)      # estimate the ATE

perms <- genperms(Z,maxiter=10000,blockvar=NULL)   # set the number of simulated random assignments

# ------------------------------------------------------
# estimate sampling dist under the sharp null that tau=0
# ------------------------------------------------------

Ys <- genouts(Y,Z,ate=0)    # create potential outcomes under the sharp null of no effect for any unit

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the implied schedule of potential outcomes implied by the null hypothesis

ate                             # estimated ATE
sum(abs(distout) >= abs(ate))   # two-tailed comparison used to calculate p-value
sum(    distout  >=     ate )   # two-tailed comparison used to calculate p-value
sort(distout)                   # list the distribution of estimates under the sharp null of no effect

dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

# ------------------------------------------------------
# estimate the sampling dist under the null that tau=ate
# ------------------------------------------------------

Ys <- genouts(Y,Z,ate=ate)    # create potential outcomes under the sharp null that the effect = estimated ATE for all units

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the implied schedule of potential outcomes implied by the null hypothesis

ate                             # estimated ATE
sum(abs(distout) >= abs(ate))   # two-tailed comparison used to calculate p-value


dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

