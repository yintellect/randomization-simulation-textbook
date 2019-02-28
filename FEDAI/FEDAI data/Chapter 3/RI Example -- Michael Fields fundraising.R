# Michael Fields: fundraising experiment 2
# An example of data analysis: estimation of the ATE, hypothesis testing, construction of confidence intervals

# Note: adjust the setwd locations as appropriate for your machine
#

rm(list=ls())       # clear objects in memory
library(ri)
set.seed(1234567)   # random number seed, so that results are reproducible

library(foreign)    # package allows R to read Stata datasets
setwd("/Users/donaldgreen/Dropbox/Consulting/Michael Fields")


fields <- read.dta("fundraising test 2 randomized with contributions added.dta")

Z <- as.integer(fields$picture_good_guy) + as.integer(fields$no_picture_good_guy)   
Y <- fields$Amount                   # donation amount

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

