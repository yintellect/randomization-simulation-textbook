# Exercise 4.5 (restricted randomization)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


setwd("~/MA/2019Spring/RAship/FEDAI/FEDAI data/Chapter 4")
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

setwd("~/MA/2019Spring/RAship/FEDAI/FEDAI code STATA")
stata_p <-readstata13::read.dta13("4_5_mar9242.dta")
stata_perm <-stata_p%>%
        select(starts_with("z"))%>%
        as.matrix()
dim(stata_perm)

perms <- genperms.custom(numiter=10000,randfun=randfun)    # notice the use of the restricted randomization function in the generation of simulated random allocations

probs <- genprob(perms) # important: restricted randomization can sometimes generate unequal probabilities of assignment, so it's important to generate the probs and use inverse probability weights when estimating the ATE
weights <- (1/probs) *Z + (1/(1-probs))*(1-Z)
var.weights.treat <- var(weights[Z==1])
var.weights.control <- var(weights[Z==0])


probs <- genprob(stata_perm) # important: restricted randomization can sometimes generate unequal probabilities of assignment, so it's important to generate the probs and use inverse probability weights when estimating the ATE
weights <- (1/probs) *Z + (1/(1-probs))*(1-Z)
var.weights.treat <- var(weights[Z==1])
var.weights.control <- var(weights[Z==0])



ate <- estate(Y,Z,prob=probs)    

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,stata_perm,prob=probs)

ate
dispdist(distout,ate)
mean(abs(distout) > abs(ate))
# repeat the estimation, this time controlling for the covariate

ateX <- estate(Y,Z,X,prob=probs)    
Ys <- genouts(Y,Z,ate=0)
distout <- gendist(Ys,stata_perm,X,prob=probs)
ateX
dispdist(distout,ateX)

mean(abs(distout) > abs(ateX))

# d)

D<-Z
perms_complete_RA <- genperms(D,maxiter=10000)
probs_complete_RA <- genprobexact(D)
ate_complete_RA <- estate(Y,D,prob=probs_complete_RA)
Ys_complete_RA <- genouts(Y,D,ate=ate_complete_RA)
distout_complete_RA <- gendist(Ys_complete_RA,perms_complete_RA,
                               prob=probs_complete_RA)
se_complete_RA <- sd(distout_complete_RA)
se_complete_RA



ate_cov_complete_RA <- estate(Y,D,X,prob=probs_complete_RA)
Ys_cov_complete_RA <- genouts(Y,D,ate=ate_cov_complete_RA)
distout_cov_complete_RA <- gendist(Ys_cov_complete_RA,perms_complete_RA,X,
                                   prob=probs_complete_RA)
se_cov_complete_RA <- sd(distout_cov_complete_RA)
se_cov_complete_RA



probs_restricted_RA <- genprob(stata_perm)
ate_restricted_RA <- estate(Y,D,prob=probs_restricted_RA)
Ys_restricted_RA <- genouts(Y,D,ate=ate_restricted_RA)
distout_restricted_RA <- gendist(Ys_restricted_RA,stata_perm,
                                 prob=probs_restricted_RA)
se_restricted_RA <- sd(distout_restricted_RA)
se_restricted_RA


ate_cov_restricted_RA <- estate(Y,D,X,prob=probs_restricted_RA)
Ys_cov_restricted_RA <- genouts(Y,D,ate=ate_cov_restricted_RA)
distout_cov_restricted_RA <- gendist(Ys_cov_restricted_RA,
                                     stata_perm,X,
                                     prob=probs_restricted_RA)
se_cov_restricted_RA <- sd(distout_cov_restricted_RA)
se_cov_restricted_RA
