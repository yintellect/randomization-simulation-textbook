# Haynes et al. texting and tax collection experiment
#
# An example of data analysis: estimation of the ATE, hypothesis testing, construction of confidence intervals

# Note: adjust the setwd locations as appropriate for your machine
#

rm(list=ls())       # clear objects in memory
setwd("/Users/donaldgreen/Dropbox/Teaching 2012-2013/Experiments/Data and R Programs/Extras")
library(ri)        # load the RI package
library(foreign)    # package allows R to read Stata datasets

set.seed(1234567)   # random number seed, so that results are reproducible

# data involve only the personal and standard texts

tax <- read.dta("Text message & tax collection subset.dta")

names(tax)

# ITT effects

Z <- tax$personal
Y <- tax$amountpaid  

# note that you can restrict the sample to compliers if you like

#  Z <- tax$personal[tax$complier==1]  
#  Y <- tax$amountpaid[tax$complier==1]               

probs <- genprobexact(Z)          # generate probability of treatment 

ate <- estate(Y,Z,prob=probs)     # estimate the ATE

perms <- genperms(Z,maxiter=100000)  # set the number of simulated random assignments

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

