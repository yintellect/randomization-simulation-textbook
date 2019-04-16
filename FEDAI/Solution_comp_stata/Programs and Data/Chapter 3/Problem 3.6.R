# Exercise 3.6 (Hajj natural experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3")

# Data are from Clingingsmith, David, Asim Ijaz Khwaja, and Michael Kremer. 2009. “Estimating the Impact of the Hajj: Religion and Tolerance in Islam’s Global Gathering.” Quarterly Journal of Economics 124: 1133-70.

hajj <- read.dta("Problem 3.6 Clingingsmith.dta")

D <- as.numeric(hajj$success =="treatment")
Y <- hajj$views   

# generate probability of treatment 
probs <- genprobexact(D)

# estimate the ATE
ate <- estate(Y,D,prob=probs) 

# set the number of simulated random assignments
perms <- genperms(D,maxiter=10000)

# create potential outcomes under the sharp null of no effect for any unit
Ys <- genouts(Y,D,ate=0) 

# generate the sampling distribution based on the 
# schedule of potential outcomes implied by the null hypothesis
distout <- gendist(Ys,perms,prob=probs)

# estimated ATE
ate

# one-tailed comparison used to calculate p-value (greater than)
sum(distout>=ate)

# two-tailed comparison used to calculate p-value
sum(abs(distout)>=ate)

p.value.onesided <- mean(distout>=ate)
p.value.twosided <- mean(abs(distout)>=ate)
  
# display p-values, 95% confidence interval, standard error under the null, 
# and graph the sampling distribution under the null
dispdist(distout,ate)               
