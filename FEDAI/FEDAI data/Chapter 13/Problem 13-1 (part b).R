# Exercise 13.1 (direct mail's effect on referendum voting)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 13/")

#  Data are from Middleton, Joel, and Todd Rogers. 2010. “Defend Oregon’s Voter Guide Program.” Report for the Analyst Institute. 

mail <- read.dta("Chapter 13_Middleton and Rogers (2010) Dataset.dta")
colnames(mail)

Z  <- as.integer(mail$treatment) - 1          # treatment
X  <- mail$dem_perf_06        # covariate: Dem voteshare in 2006
Y  <- mail$relevant_measures_net
N <- length(Z)


# set up a restricted randomization whereby random allocations (Zri) are discarded if they generate an absolute coefficient smaller than 0.5 when the covariate X is regressed on Zri (which is the same as saying that the treatment and control means in terms of X are closer than 0.5 percentage points)

randfun <- function() {
	teststat <- 100
	while (teststat > 0.5) {
		Zri <- sample(Z)
		teststat <- summary(lm(X~Zri))$coefficients[2,1]   # extract the coefficient
	}
	return(Zri)
}

perms <- genperms.custom(numiter=10000,randfun=randfun)    # notice the use of the restricted randomization function in the generation of simulated random allocations

probs <- genprob(perms)           # important: restricted randomization can sometimes generate unequal probabilities of assignment, so it's important to generate the probs and use inverse probability weights when estimating the ATE

ate <- estate(Y,Z,prob=probs)    

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate

dispdist(distout,ate)



