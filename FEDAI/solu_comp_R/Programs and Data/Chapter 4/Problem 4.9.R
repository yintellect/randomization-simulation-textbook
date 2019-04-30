# Exercise 4.9 

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

phones <- read.dta("Problem 4.9 Gerber and Green.dta")

#### Problem 4.9c ####

ests <- c(.00964, -.007829, -.01362,.008271)
shareoftotalN <- c(0.049487, 0.1520981, 0.626616, 0.171799)
overall_ate <-sum(ests*shareoftotalN)
overall_ate


#### Problem 4.9e ####

Y <- phones$vote02
block <- phones$strata
Z <- phones$treat2

## Proportion of subjects in each block assigned to treatment
block.pr <- tapply(Z, block, mean)

q <- rep(NA, length(Y))

for(i in 1:4){
  q[block==i] <- block.pr[i]*Z[block==i] + (1-block.pr[i])*(1-Z[block==i])
}

fit <- lm(Y ~ Z, weights=1/q)
summary(fit)

