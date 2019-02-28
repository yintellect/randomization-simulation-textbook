
#   -----------------------------------------------------------------------
#   Computer practicum using the Titiunik data (Feb 28 class)
#   -----------------------------------------------------------------------

#   --------------------------
#   Preparation
#   ---------------------------
rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
setwd("~/Desktop/2016/4368 Experimental/5.FEDAI replication/Chapter 3")
library(foreign)    # package allows R to read Stata datasets
titiunik <- read.dta("Titiunik data for Exercises to Chapter 3.dta")

#   --------------------------
#   Load schedule
#   --------------------------

# Z= assign treatment
Z <-  titiunik$term2year  
# Y= observered outcome
Y <- titiunik$bills_introduced
# block = {0(texas), 1(arkansas)}
block <- titiunik$texas0_arkansas1   

# probabilities of being treated with each block
probs <- genprobexact(Z,blockvar=block) 
# print the different probabilities in each block
table(probs)

# estimate the ATE,use IWP weight each observations in different blocks
ate <- estate(Y,Z,prob=probs) 
# unbiased ATE
ate

#   --------------------------
#   Randomization Inference
#   --------------------------

# specify block and the algorithm can weight each observation
perms <- genperms(Z,maxiter=10000,blockvar=block)   

# generate the complete schedule under sharp null
Ys <- genouts(Y,Z,ate=0)  

# hypothetically replicate 10000 times, randomly choose in the permutations
# probabilities vary across block, weight each observation using IPW
distout <- gendist(Ys,perms,prob=probs) 
# now we have 10000 hypothetical ATE to draw sampling distribution
distout 


# The estimated ATE obtained from sample
ate  
# Two-tailed comparison used to calculate p-value
mean(abs(distout) >= abs(ate)) 

# display p-values, 95% confidence interval, standard error under the null, 
#                       and graph the sampling distribution under the null
dispdist(distout,ate)      

#   --------------------------
#   Weighted least square
#   --------------------------
# generate IPW weights 
ipw <- (1-block)*(Z/mean(Z[block==0]) + (1-Z)/(1-mean(Z[block==0]))) + 
       (block)  *(Z/mean(Z[block==1]) + (1-Z)/(1-mean(Z[block==1])))

# weighted regression with IPW weights

# unbiased but large standard error
summary(lm(Y ~ Z , weights = ipw))


# naive estimate (ignoresd blocking with unequal probabilities of treatment)
mean(Y[Z==1])-mean(Y[Z==0])

# Least Square with Dummy Variables
# close but not necessarily unbiased
summary(lm(Y ~ Z + block))

# includes both weights and block dummy
# same estimated ATE, different standard errors (because blocks predict Y)
# The p-value here matches RI quite closely
summary(lm(Y ~ Z + block, weights = ipw))
