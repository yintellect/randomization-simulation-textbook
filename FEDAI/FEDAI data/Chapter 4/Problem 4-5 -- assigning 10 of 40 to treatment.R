# Exercise 4.5 (restricted randomization)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


setwd("~/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")

#  Data are from Table 4.1

teach <- read.dta("Teachers data for Table 4-1.dta")
colnames(teach)

# Z  <- teach$D       # treatment in Table 4.1

dvec <- rep.int(c(0,0,0,1),10)
Z <- sample(dvec)   # alternative treatment of 10 of 40 subjects
Y1 <- teach$y1      # treated potential outcome
Y0 <- teach$y0      # untreated potential outcome
X <- teach$x        # pre-test

Y <- Y0*(1-Z) + Y1*(Z)    # observed outcomem given random assignment
N <- length(Z)

# Part (a)
# set up a restricted randomization whereby random allocations (Zri) are discarded if they generate an F-statistic whose p-value is smaller than 0.05 when Zri is regressed on the covariate X

randfun <- function() {
	teststat <- -1
	while (teststat < 0.05) {
		Zri <- sample(Z)
		fstat <- summary(lm(Zri~X))$fstatistic
		teststat <- pf(fstat[1],fstat[2],fstat[3],lower.tail=FALSE)   # extract the p-value from the F-test
			}
	return(Zri)
}

perms <- genperms.custom(numiter=100000,randfun=randfun)    # notice the use of the restricted randomization function in the generation of simulated random allocations

probs <- genprob(perms)           # important: restricted randomization can sometimes generate unequal probabilities of assignment, so it's important to generate the probs and use inverse probability weights when estimating the ATE

plot(X,probs)  # inspect probabilities of assignment to see if they are related to X

ate <- estate(Y,Z,prob=probs)    

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate
dispdist(distout,ate)



