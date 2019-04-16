# Exercise 10.4 (Indian reservations for women natural experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

# Set your working directory
# setwd("")

# Data are from Bhavnani, Rikhil R. 2009. 
# “Do Electoral Quotas Work after They Are Withdrawn? 
# Evidence from a Natural Experiment in India.” 
# American Political Science Review 103: 23-35.

bhav <- na.omit(read.dta("Chapter 10_Bhavnani (2009) Dataset.dta"))


#### Problem 10.4a ####

# treatment: reservations for women candidates
Z <- as.integer(bhav$controltreat) - 1
# an intermediate outcome: turnout
Y <- bhav$turnout                          

# generate probability of treatment 
probs <- genprobexact(Z)                
# estimate the ITT (ATE of assignment to reservation)
ate <- estate(Y,Z,prob=probs)           

numiter <- 10000
perms <- genperms(Z,maxiter=numiter)

# create potential outcomes under the sharp null of no effect for any unit
Ys <- genouts(Y,Z,ate=0)

# generate the sampling distribution based on the 
# schedule of potential outcomes implied by the null hypothesis
distout <- gendist(Ys,perms,prob=probs)    
# obtain p value
p.value.two.sided <- mean(abs(distout) >= abs(ate))

ate
p.value.two.sided 

#### Problem 10.4b ####

testvar <- var(Y[Z==1]) - var(Y[Z==0])

varlist <- rep(NA,numiter)

for (i in 1:dim(perms)[2]) {  
  Zri <- perms[,i]
  varlist[i] <- var(Y[Zri==1]) - var(Y[Zri==0])
}
# p-value for one-tailed comparison
mean(varlist >= testvar)             
# p-value for testing unequal variances
mean(abs(varlist) >= abs(testvar))   
