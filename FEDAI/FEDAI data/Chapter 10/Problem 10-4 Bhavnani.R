# Exercise 10.4 (Indian reservations for women natural experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 10")

# Data are from Bhavnani, Rikhil R. 2009. “Do Electoral Quotas Work after They Are Withdrawn? Evidence from a Natural Experiment in India.” American Political Science Review 103: 23-35.

bhav <- na.omit(read.dta("Chapter 10_Bhavnani (2009) Dataset.dta"))
colnames(bhav)
Z <- as.integer(bhav$controltreat) - 1     # treatment: reservations for women candidates
Y <- bhav$turnout                          # an intermediate outcome: turnout


probs <- genprobexact(Z)                # generate probability of treatment 

ate <- estate(Y,Z,prob=probs)           # estimate the ITT (ATE of assignment to reservation)

numiter <- 10000
perms <- genperms(Z,maxiter=numiter)

Ys <- genouts(Y,Z,ate=0)       # create potential outcomes under the sharp null of no effect for any unit

distout <- gendist(Ys,perms,prob=probs)    # generate the sampling distribution  based on the schedule of potential outcomes implied by the null hypothesis

ate
sum(distout >= ate)
sum(abs(distout) >= abs(ate))

dispdist(distout,ate)      # display p-values, 95% confidence interval, standard error under the null, and graph the sampling distribution under the null


# next, test for unequal variances using RI

testvar <- var(Y[Z==1]) - var(Y[Z==0])

varlist <- rep(NA,numiter)

for (i in 1:dim(perms)[2]) {	
	Zri <- perms[,i]
	varlist[i] <- var(Y[Zri==1]) - var(Y[Zri==0])
}

mean(varlist >= testvar)             # p-value for one-tailed comparison
mean(abs(varlist) >= abs(testvar))   # p-value for testing unequal variances