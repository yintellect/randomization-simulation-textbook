# Exercise 6.10 (Election monitoring experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
library(foreign)    # package allows R to read Stata datasets
set.seed(1234567)   # random number seed, so that results are reproducible

# Set your working directory
# setwd("")

# Data are from Hyde, Susan. 2010. “Experimenting in Democracy Promotion: 
# International Observers and the 2004 Presidential Elections in Indonesia.”
# Perspectives on Politics 8:511-27.

hyde <- read.dta("Problem 6.10 Hyde.dta")

#### Problem 6.10d ####

# monitoring treatment assignment
Z <- as.integer(hyde$Sample) -1  

# monitoring treatment received
D <- as.numeric(hyde$observed=="yes")

# Outcome variable is invalid ballots
Y <- hyde$invalidballots

# generate probability of treatment assignment
probs <- genprobexact(Z)  

# estimate the ITT (ATE of assignment on outcome)
ITT <- estate(Y,Z,prob=probs) 

# estimate the ITTd (ATE of assignment on treatment receipt)
ITTd <- estate(D,Z,prob=probs) 

# Estimate the CACE
CACE <- ITT/ITTd
ITT
CACE

#### Problem 6.10e ####

# Generate permutation matrix
perms <- genperms(Z,maxiter=10000)  

# Generate outcomes under sharp null
Ys <- genouts(Y,Z,ate=0)   

# Generate sampling distribution under sharp null
distout <- gendist(Ys,perms,prob=probs) 

# Two-sided p-value
p.value.twosided <- mean(abs(distout) >= abs(ITT))
p.value.twosided

