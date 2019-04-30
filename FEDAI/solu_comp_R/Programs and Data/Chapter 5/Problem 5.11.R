# Exercise 5.11 -- Nickerson Voter Mobilization

rm(list=ls())       # clear objects in memory

#### Problem 5.11a ####

# Reproduce Dataset
Z <- c(rep("baseline", 2572), rep("treatment", 486+2086), rep("placebo", 470+2109))
D <- c(rep(0, 2572), rep(1, 486), rep(0,2086), rep(1, 470), rep(0,2109))
Y <- c(rep(1, round(2572*0.3122)), rep(0,round(2572*(1-0.3122))),
       rep(1, round(486*0.3909)), rep(0,round(486*(1-0.3909))),
       rep(1, round(2086*0.3274)), rep(0,round(2086*(1-0.3274))),
       rep(1, round(470*0.2979)), rep(0,round(470*(1-0.2979))),
       rep(1, round(2109*0.3215)), rep(0,round(2109*(1-0.3215))))

# Estimate Compliance in Treatment and Placebo Groups
pr.c.treatment <- mean(D[Z=="treatment"])
pr.c.treatment
pr.c.placebo <- mean(D[Z=="placebo"])
pr.c.placebo

#### Problem 5.11b ####

# Estimate the proportion of nevertakers in treatment and placebo
rate.nt.treatment <- mean(Y[Z=="treatment" & D==0])
rate.nt.placebo <- mean(Y[Z=="placebo" & D==0])
rate.nt.treatment
rate.nt.placebo

#### Problem 5.11c ####

# Estimate the effects of the placebo treatment
itt.placebo <- mean(Y[Z=="placebo"]) - mean(Y[Z=="baseline"])
cace.placebo <- itt.placebo/pr.c.placebo

#### Problem 5.11d ####

# Two methods for estimating the CACE

## Method 1: ITT/ITTd
itt.treatment <- mean(Y[Z=="treatment"]) - mean(Y[Z=="baseline"])
cace.treatment1 <- itt.treatment/pr.c.treatment
cace.treatment1

## Method 2: Conditioning on compliance, compare treated to placebo.
cace.treatment2 <- mean(Y[Z=="treatment" & D==1]) - mean(Y[Z=="placebo" & D==1])
cace.treatment2