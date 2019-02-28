# NYC experiment: Green, Krasno, and Panagopoulos 2013

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible


setwd("/Users/donaldgreen/Dropbox/Teaching 2013-2014/Experiments/Data and R Programs/Chapter 3/")

library(foreign)    # package allows R to read Stata datasets

# Data are from Green, Krasno, and Panagopoulos 2014 MPSA project

nyc <- read.dta("NYC matching funds 2014 small.dta")
names(nyc)
Z <-  nyc$treat_info       # treatment is info about matching fund system
Y <- nyc$post_treat_amnt
block <- nyc$block2   # randomization occurs within each block of propensity to donate

table(Z,block)

mean(Y[Z==1])
mean(Y[Z==0])

probs <- genprobexact(Z,blockvar=block)   # blocking is assumed when generating probability of treatment
table(probs)

ate <- estate(Y,Z,prob=probs)      # estimate the ATE

perms <- genperms(Z,maxiter=500,blockvar=block)   # set the number of simulated random assignments


Ys <- genouts(Y,Z,ate=0)    # create potential outcomes under the sharp null of no effect for any unit

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate                             # estimated ATE
mean(abs(distout) >= abs(ate))  # two-tailed comparison used to calculate p-value


dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null



