# Exercise 3.8 (Legislative terms natural experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3/")

library(foreign)    # package allows R to read Stata datasets

# Data are from Titiunik, Rocío. 2010. 
# “Drawing Your Senator from a Jar: Term Length and Legislative Behavior.” 
# University of Michigan Working Paper. 
titiunik <- read.dta("Problem 3.8 Titiunik.dta")

#### Problem 3.8a ####
D <-  titiunik$term2year       
Y <- titiunik$bills_introduced
block <- titiunik$texas0_arkansas1 

ate_texas <- mean(Y[D==1 & block==0]) - mean(Y[D==0 & block==0])
ate_arkansas <- mean(Y[D==1 & block==1]) - mean(Y[D==0 & block==1])

ate_texas
ate_arkansas

#### Problem 3.8b ####
se_texas = sqrt(var(Y[D==0 & block==0])/length(Y[D==0 & block==0]) +
                  var(Y[D==1 & block==0])/length(Y[D==1 & block==0]))

se_arkansas = sqrt(var(Y[D==0 & block==1])/length(Y[D==0 & block==1]) +
                     var(Y[D==1 & block==1])/length(Y[D==1 & block==1]))
se_texas
se_arkansas

#### Problem 3.8c ####
ate_overall <- length(Y[block==0])/length(Y) *ate_texas + 
  length(Y[block==1])/length(Y) *ate_arkansas
ate_overall


#### Problem 3.8e ####
se_overall= sqrt((length(Y[block==0])/length(Y))^2 *se_texas^2 + 
                   (length(Y[block==1])/length(Y))^2 *se_arkansas^2)
se_overall

#### Problem 3.8f ####

# Note differential probabilities
probs <- genprobexact(D,blockvar=block) 

# Estimate ATE
ate <- estate(Y,D,prob=probs)

# Note blocked randomization
perms <- genperms(D,maxiter=10000,blockvar=block) 

# Generate potential outcomes under sharp null
Ys <- genouts(Y,D,ate=0)

# Simulate sampling distribution under sharp null
distout <- gendist(Ys,perms,prob=probs)  
p.value.twosided <- mean(abs(distout) >= abs(ate)) 

ate                             
p.value.twosided


