# Exercise 3.8 (Legislative terms natural experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3/")

library(foreign)    # package allows R to read Stata datasets

# Data are from Titiunik, Rocío. 2010. “Drawing Your Senator from a Jar: Term Length and Legislative Behavior.” University of Michigan Working Paper. 

titiunik <- read.dta("Titiunik data for Exercises to Chapter 3.dta")

Z <-  titiunik$term2year       # treatment is 2 year rather than 4 year term
Y <- titiunik$bills_introduced
block <- titiunik$texas0_arkansas1   # randomization occurs within each state

probs <- genprobexact(Z,blockvar=block)   # blocking is assumed when generating probability of treatment
table(probs)

ate <- estate(Y,Z,prob=probs)      # estimate the ATE

perms <- genperms(Z,maxiter=10000,blockvar=block)   # set the number of simulated random assignments


Ys <- genouts(Y,Z,ate=0)    # create potential outcomes under the sharp null of no effect for any unit

distout <- gendist(Ys,perms,prob=probs)  # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate                             # estimated ATE
mean(abs(distout) >= abs(ate))  # two-tailed comparison used to calculate p-value


dispdist(distout,ate)       # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null


# illustration that naive estimation of the ATE is misleading


mean(Y[Z==1 & block==0])
mean(Y[Z==0 & block==0])

mean(Y[Z==1 & block==1])
mean(Y[Z==0 & block==1])

mean(block==0)
mean(block==1)

# put the formula together to create a block-by-block weighted avg
mean(block==0)*(mean(Y[Z==1 & block==0])-mean(Y[Z==0 & block==0])) + mean(block==1)*(mean(Y[Z==1 & block==1])-mean(Y[Z==0 & block==1]))

# generate IPW weights
ipw <- (1-block)*(Z/mean(Z[block==0]) + (1-Z)/(1-mean(Z[block==0]))) + 
       (block)  *(Z/mean(Z[block==1]) + (1-Z)/(1-mean(Z[block==1])))

# weighted regression with IPW weights
summary(lm(Y ~ Z , weights = ipw))

# naive estimate (ignoresd blocking with unequal probabilities of treatment)
mean(Y[Z==1])-mean(Y[Z==0])
# LSDV estimate
summary(lm(Y ~ Z + block))

# note what happens when one includes both weights and block dummy
# same estimated ATE, different standard errors (because blocks predict Y)
# notice that the p-value here matches RI quite closely
summary(lm(Y ~ Z + block, weights = ipw))