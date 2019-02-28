# Exercise 5.10 -- Beijing Clustered Design

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible



library(foreign)    # package allows R to read Stata datasets

# Data are from Guan, Mei, and Donald Green. 2006. “Non-Coercive Mobilization in State-Controlled Elections: An Experimental Study in Beijing.” Comparative Political Studies 39:1175-93.

beijing.all <- read.dta("Chapter 5_Guan and Green (2006) Dataset.dta")
colnames(beijing.all)

library(AER)
cace_fit <- ivreg(turnout ~ contact, ~ treat2, data = beijing.all)
library(lmtest)
coeftest(cace_fit, vcovHC(cace_fit))


# get rid of a couple of observations with missing outcome data
beijing <- na.omit(beijing.all)

Z <-     beijing$treat2
Y <-     beijing$turnout
clust <- beijing$dormid

# conduct randomization inference on the ITT (a rejection of the null that ITT=0 also implies a rejection of the null that the CACE=0)

probs <- genprobexact(Z,clustvar=clust)  # subjects are clustered by dorm room

numiter <- 10000

perms <- genperms(Z,maxiter=numiter,clustvar=clust)    # clustered assignment

ate <- estate(Y,Z,prob=probs)

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate                                  # estimated ATE
sum(distout <= ate)                  # one-tailed comparison
sum(abs(distout) >= abs(ate))        # two-tailed comparison

dispdist(distout,ate)  
