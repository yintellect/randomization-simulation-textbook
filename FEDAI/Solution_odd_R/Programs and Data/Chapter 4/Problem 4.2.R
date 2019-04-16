# Exercise 4.2 (Game-playing experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")

# Data are from Dan Gendelman (www.eshcolot.com), 
# who studied the effects of a teaching curriculum on 
# elementary students' performance at the puzzle called "Rush Hour"

rush <- read.dta("Problem 4.2 Gendelman.dta")

#### Excercise 4.2a ####

# treatment is thinking strategies curriculum
D <- rush$treat      
# number of puzzled solved during the testing session
Y <- rush$posttest
# covariate is the pretest score
X <- rush$pretest

# RI: randomization check, testing the effect of the covariate on the treatment

# set the number of simulated random assignments 
# (if you set it to 48620 or higher, your results 
# will be exact because that is the true number of possible random assignments)
perms <- genperms(D,maxiter=50000)  

# reset numiter so that it is no larger than the maximum number of possible randomizations
numiter <- ncol(perms)

# observed F statistic
Fstat <- summary(lm(D~X))$fstatistic[1] 

# initialize vector of simulated F statistics
Fstatstore <- rep(NA,numiter) 

for (i in 1:numiter) {
  Fstatstore[i] <- summary(lm(perms[,i]~X))$fstatistic[1]
}

p.value <- mean(Fstatstore >= Fstat) 
p.value

#### Exercise 4.2b ####

probs <- genprobexact(D)
ate <- estate(Y,D,prob=probs)
ate
Ys <- genouts(Y,D,ate=ate) 
distout <- gendist(Ys,perms,prob=probs)
ci.95 <- quantile(distout,probs=c(0.025, 0.975))
ci.95

#### Exercise 4.2c ####
Y.improve <- rush$improvement
ate.improve <- estate(Y.improve,D,prob=probs)
ate.improve
Ys <- genouts(Y.improve,D,ate=ate.improve) 
distout <- gendist(Ys,perms,prob=probs)
ci.95.improve <- quantile(distout,probs=c(0.025, 0.975))
ci.95.improve
