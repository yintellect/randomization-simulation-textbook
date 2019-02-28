# Exercise 6.10 (Election monitoring experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

# Data are from Hyde, Susan. 2010. “Experimenting in Democracy Promotion: International Observers and the 2004 Presidential Elections in Indonesia.” Perspectives on Politics 8:511-27.

setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 6")

hyde <- read.dta("Chapter 6_Hyde (2010) Dataset.dta")

Z <- as.integer(hyde$Sample) -1   # monitoring treatment
Y <- hyde$invalidballots

probs <- genprobexact(Z)          # generate probability of treatment assignment
ate <- estate(Y,Z,prob=probs)     # estimate the ITT (ATE of assignment)

perms <- genperms(Z,maxiter=10000)  # set the number of simulated random assignments

Ys <- genouts(Y,Z,ate=0)       # create potential outcomes under the sharp null of no effect for any unit

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate                             # report the estimated ITT (ATE of assignment)
sum(distout >= ate)
sum(abs(distout) >= abs(ate))

dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

