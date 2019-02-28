# Hajj natural experiment -- Clingingsmith et al. subset
#
# An example of data analysis: estimation of the ATE, hypothesis testing, construction of confidence intervals

# Note: adjust the setwd locations as appropriate for your machine
#

rm(list=ls())       # clear objects in memory
library(ri)
set.seed(1234567)   # random number seed, so that results are reproducible

library(foreign)    # package allows R to read Stata datasets
setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3")

# Data are from Clingingsmith, David, Asim Ijaz Khwaja, and Michael Kremer. 2009. “Estimating the Impact of the Hajj: Religion and Tolerance in Islam’s Global Gathering.” Quarterly Journal of Economics 124: 1133-70.

hajj <- read.dta("Clingingsmith subset.dta")

Z <- as.integer(hajj$success) - 1   # convert treat to an indicator: 1=treat
Y <- hajj$views                   # sum of views toward various national groups

probs <- genprobexact(Z)          # generate probability of treatment 

ate <- estate(Y,Z,prob=probs)     # estimate the ATE

perms <- genperms(Z,maxiter=10000)  # set the number of simulated random assignments

Ys <- genouts(Y,Z,ate=0)            # create potential outcomes UNDER THE SHARP NULL OF NO EFFECT FOR ANY UNIT

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate                                 # estimated ATE
sum(distout >= ate)                 # one-tailed comparison used to calculate p-value (greater than)
sum(abs(distout) >= abs(ate))       # two-tailed comparison used to calculate p-value

dispdist(distout,ate)               # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

#--------------------------------------------------------------
# estimation of confidence intervals assuming ATE=estimated ATE
#--------------------------------------------------------------
Ys <- genouts(Y,Z,ate=ate)            # create potential outcomes UNDER THE ASSUMPTION THAT ATE=ESTIMATED ATE

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

dispdist(distout,ate)               # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null

