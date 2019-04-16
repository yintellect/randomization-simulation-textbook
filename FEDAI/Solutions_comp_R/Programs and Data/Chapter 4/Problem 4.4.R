# Exercise 4.4 (teachers experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")

#  Data are from Table 4.1

teach <- read.dta("Problem 4.4 Teachers.dta")

#### Problem 4.4a ####

D  <- teach$D    
Y1 <- teach$y1   
Y0 <- teach$y0
X <- teach$x

Y <- Y0*(1-D) + Y1*(D)    # Equation 2.2

# Two equivalent ways to estimate the ATE: Regression and Difference-in-means

# Regression
fit <- lm(Y~D)
summary(fit)

# Difference in means
diff_means <- mean(Y[D==1])-mean(Y[D==0])
diff_means

#### Problem 4.4b ####

# a diagnostic test to assess whether rescaling the outcome as a change 
# rather than a level is likely to improve the precision with which the ATE is estimated (see equation 4.6)
fit.1 <- lm(Y~X,subset=D==1)
fit.0 <- lm(Y~X,subset=D==0)

sum_of_coefficients <- fit.1$coefficients[2] + fit.0$coefficients[2]
sum_of_coefficients

Ydiff <- Y-X
fit.diff <- lm(Ydiff~D)

#### Problem 4.4c ####

# estimate the ATE using covariate adjustment
fit.cov <- lm(Y~D+X)
summary(fit.cov)   

#### Problem 4.4d ####

# simulate possible random allocations
perms <- genperms(Z,maxiter=10000)  
probs <- genprobexact(Z)

# estimate the ATE
ate <- estate(Y,Z,prob=probs)       

# calculate potential outcomes under sharp null of 0 effect for all units
Ys <- genouts(Y,Z,ate=0)            

# generate the sampling distributionbased on the 
# schedule of potential outcomes implied by the null hypothesis
distout <- gendist(Ys,perms,prob=probs)  

# report the estimated ATE
ate                                 

# display p-values, 95% confidence interval, 
# standard error under the null, 
# and graph the sampling distribution under the null
dispdist(distout,ate)               

#### Problem 4.4e ####

# estimate the ATE using covariate adjustment
ate_cov <- estate(Y,D,X,prob=probs)

# generate the sampling distribution based on the 
# schedule of potential outcomes implied by the null hypothesis
distout_cov <- gendist(Ys,perms,X,prob=probs)
p.value_cov <- mean(abs(distout_cov)>abs(ate_cov)) 

# report the estimated ATE
ate_cov
p.value_cov

#### Problem 4.4f ####

# generate potential outcomes assuming that the 
# true treatment effect for every unit is equal to the estimated ATE
Ys <- genouts(Y,D,ate=ate)  
distout <- gendist(Ys,perms,prob=probs)
ci.95 <- quantile(distout, probs=c(0.025, .975))
ci.95

#### Problem 4.4g ####

# generate potential outcomes assuming that the true treatment effect 
# for every unit is equal to the estimated ATE under covariate adjustment
Ys_cov <- genouts(Y,D,ate=ate_cov)  
distout_cov <- gendist(Ys_cov,perms,X,prob=probs)
ci.95_cov <- quantile(distout_cov, probs=c(0.025, .975))
ci.95_cov

