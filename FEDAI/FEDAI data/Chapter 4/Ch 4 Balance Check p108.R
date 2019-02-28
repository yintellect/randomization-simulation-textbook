#   ---------------------------------
#    RI for balance check  P.108
#    (using Example exercise 3.9)
#   ---------------------------------

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
setwd("~/Desktop/2016/4368 Experimental/5.FEDAI replication/Chapter 3")
library(foreign)    # package allows R to read Stata datasets

# read data
camerer <- read.dta("Camerer data for Chapter 3 exercises.dta")
# check variables in data frame
colnames(camerer)

# Z= assigned trearment
Z <-  camerer$treatment
# observed outcomes
Y <- camerer$experimentbet
# block in the experiment
block <- camerer$pair       

#   -----------------------------------------------------------------------

# reads in covariates: in this case is a pre-test score
covs <- as.matrix(camerer$preexperimentbets) 
summary(lm(Z~covs))

# specify the probabilities of treatment assgin
# must follow the exact procedure you used when rndomly allocaing subjects 
# (take block into account)
probs <- genprobexact(Z,blockvar=block)

#   -----------------------------------------------------------------------

# simulate the random assignment procedure many times
numiter <- 10000 # set the maxium simulation times for time-saving in computing
# generate the possible permutations
perms <- genperms(Z,maxiter=numiter,blockvar=block)
# reset numiter so that it is no larger than the maximum number of possible randomizations
numiter <- ncol(perms)  


#   -----------------------------------------------------------------------

# Use F-test to assess the null hypothesis that 
# the covariates predict random assignment (Z) no better than would be expected by chance
# （the covariate have no effect on the assigned treatment)



# a regression of the assigned treatment on all of the covariates 
#       and calculation of the F-statistic

# F-statistic from actual data 
Fstat <- summary(lm(Z~covs))$fstatistic[1]   


# calculation of the F-statistic for the 10000 times simulation

# the collection of F-statistics represents the exact sampling distribution 
# under the null hypothesis of random assignment
Fstatstore <- rep(NA,numiter)
for (i in 1:numiter) {
        Fstatstore[i] <- summary(lm(perms[,i]~covs))$fstatistic[1]  # F-statistic under the null of random assignment of Z
}

hist(Fstatstore)
abline(v =0.01892647,col="red",lty = 3,lwd=2)
# p-value
mean(Fstatstore >= Fstat)                     


linemean(Fstatstore >= Fstat)                     # p-value