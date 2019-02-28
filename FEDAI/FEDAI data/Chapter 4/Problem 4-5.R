# Exercise 4.5 (restricted randomization)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")

#  Data are from Table 4.1

teach <- read.dta("Teachers data for Table 4-1.dta")
colnames(teach)

Z  <- teach$D       # treatment
Y1 <- teach$y1      # treated potential outcome
Y0 <- teach$y0      # untreated potential outcome
X <- teach$x        # pre-test

Y <- Y0*(1-Z) + Y1*(Z)    # observed outcomem given random assignment
N <- length(Z)

summary(lm(Z~X))$fstatistic[1]

# Part (a)
# set up a restricted randomization whereby random allocations (Zri) are discarded if they generate an F-statistic whose p-value is smaller than 0.05 when Zri is regressed on the covariate X

randfun <- function() {
	teststat <- -1
	while (teststat < 0.05) {
		Zri <- sample(Z)
		teststat <- summary(lm(Zri~X))$coefficients[2,4]   # extract the p-value from the t-test (which is the same as the F-test since there is one covariate)
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

# repeat the estimation, this time controlling for the covariate

ateX <- estate(Y,Z,X,prob=probs)    
Ys <- genouts(Y,Z,ate=0)
distout <- gendist(Ys,perms,X,prob=probs)
ateX
dispdist(distout,ateX)


