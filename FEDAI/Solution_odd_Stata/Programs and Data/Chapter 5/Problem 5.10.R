# Exercise 5.10 -- Beijing Clustered Design

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
library(foreign)    # package allows R to read Stata datasets
set.seed(1234567)   # random number seed, so that results are reproducible

# Set your working directory
# setwd("")

# Data are from Guan, Mei, and Donald Green. 2006. 
# “Non-Coercive Mobilization in State-Controlled Elections: 
# An Experimental Study in Beijing.” Comparative Political Studies 39:1175-93.

beijing.all <- read.dta("Problem 5.10 Guan and Green.dta")

#### Problem 5.10a ####

# get rid of a couple of observations with missing outcome data
beijing <- na.omit(beijing.all)
Z <-     beijing$treat2
Y <-     beijing$turnout
D <-     beijing$contact
clust <- beijing$dormid

ITT <- mean(Y[Z==1]) - mean(Y[Z==0])
ITT

#### Problem 5.10b ####

# conduct randomization inference on the ITT 
# (a rejection of the null that ITT=0 also implies a rejection of the null that the CACE=0)

# subjects are clustered by dorm room
probs <- genprobexact(Z,clustvar=clust)  
itt <- estate(Y,Z,prob=probs)
itt

numiter <- 10000

# clustered assignment
perms <- genperms(Z,maxiter=numiter,clustvar=clust)    
Ys <- genouts(Y,Z,ate=0)
distout <- gendist(Ys,perms,prob=probs)

# two-tailed comparison
p.value <- sum(abs(distout) >= abs(itt))        
p.value 


#### Problem 5.10c ####

itt <- estate(Y,Z,prob=probs)
itt

ittd <- estate(D,Z,prob=probs)
ittd

cace <- itt/ittd
cace


