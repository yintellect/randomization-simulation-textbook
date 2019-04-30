# Exercise 9.7 -- Tipping, Happyfaces, and Waitstaff Sex

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

# Set your working directory
# setwd("")

# Data are from Rind, Bruce and Prashant Bordia. 1996. 
# “Effect on Restaurant Tipping of Male and Female Servers Drawing a Happy, Smiling Face on the Backs of Customers’ Checks.” 
# Journal of Applied Social Psychology. 26: 218-225.

rindb <- read.dta("Problem 9.6 Rind and Bordia.dta")

#### Problem 9.6a ####

# generate a treatment indicator
Z <- as.integer(rindb$happyface) - 1  
Y <- rindb$tip

probs <- genprobexact(Z)
ate <- estate(Y,Z,prob=probs)

numiter <- 10000
set.seed(343)
perms <- genperms(Z,maxiter=numiter)

# generate a schedule of potential outcomes under the assumption 
# that the treatment effect equals the estimated ATE for all subjects
Ys <- genouts(Y,Z,ate=ate)            

# compare variances in treatment and control groups
testvar <- var(Y[Z==1]) - var(Y[Z==0])   
vardist <- rep(NA,numiter)               

# generate the sampling distribution of the difference in variances
for (i in 1:numiter){
  vardist[i] <- var(Y[perms[,i]==1]) - var(Y[perms[,i]==0]) 
}

# p-value for var(Y1)>Var(Y0)
mean(vardist >= testvar)           
# p-value for var(Y1)<>Var(Y0)
mean(abs(vardist) >= abs(testvar)) 


#### Problem 9.6b ####
# generate indicator of waitstaff sex
female <- as.integer(rindb$female) - 1   

# regression with interaction between happyface and waitstaff sex
lmmodelint <- lm(Y~Z+female+Z*female)      
# regression model without interaction
lmmodel <- lm(Y~Z+female)    

summary(lmmodelint)

#Confirm p-value with RI

# use estimated coefficients from base model to impute potential outcomes
Y00 <- Y - lmmodel$coefficients["Z"]*Z - lmmodel$coefficients["female"]*female
Y10 <- Y00 + lmmodel$coefficients["Z"]
Y01 <- Y00 + lmmodel$coefficients["female"]
Y11 <- Y00 + lmmodel$coefficients["Z"] + lmmodel$coefficients["female"] 

f.obs <- waldtest(lmmodelint,lmmodel)$F[2]
f.sims <- rep(NA,numiter)

for (i in 1:numiter) {
  Z.sim <- perms[,i]
  
  # realized values of Y reflect single or compound treatments 
  # and the potential outcomes that they reveal
  Y.sim <- Y00
  Y.sim[Z.sim == 0 & female == 1] <- Y01[Z.sim == 0 & female == 1]
  Y.sim[Z.sim == 1 & female == 0] <- Y10[Z.sim == 1 & female == 0]
  Y.sim[Z.sim == 1 & female == 1] <- Y11[Z.sim == 1 & female == 1]
  
  # regressions based on two nested models: with and without interaction
  lmmodelint.sim <- lm(Y.sim~Z.sim + female + female*Z.sim)
  lmmodel.sim <- lm(Y.sim~Z.sim + female)
  # calculate the F-statistic by comparing two nested models
  f.sims[i] <- waldtest(lmmodelint.sim,lmmodel.sim)$F[2]	
}

# calculate the p-value by comparing the observed F-statistic 
# to the F-statistic under the null of constant & additive effects
mean(f.sims >= f.obs)   