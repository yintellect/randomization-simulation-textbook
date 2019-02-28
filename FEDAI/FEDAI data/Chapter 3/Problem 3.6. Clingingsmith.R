# Exercise 3.6 (Hajj natural experiment)
# -----------------------------------------------------------------------
# Load data and package 
# -----------------------------------------------------------------------

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

# Data are from Clingingsmith, David, Asim Ijaz Khwaja, and Michael Kremer. 2009. 
# “Estimating the Impact of the Hajj: Religion and Tolerance in Islam’s Global Gathering.” Quarterly Journal of Economics 124: 1133-70.
hajj <- read.dta("Clingingsmith subset.dta")


# -----------------------------------------------------------------------
# Set treatment and outcome 
# -----------------------------------------------------------------------

# treatment indicator
Z <- as.integer(hajj$success) - 1 # as.integer convert "control" to 1, "treatment" to 2
# outcome variable
Y <- hajj$views 

# generate probability of treatment 
probs <- genprobexact(Z)
# check
table(probs)

# estimate the ATE
ate <- estate(Y,Z,prob=probs)     
ate

# generate all permutations of Z under complete random assignmnet
perms <- genperms(Z,maxiter=10000)  
# if permutation more than 10000, do 10000 permutations

# -----------------------------------------------------------------------
# Randomization Inference 
# -----------------------------------------------------------------------

# create complete schedule UNDER THE SHARP NULL OF NO EFFECT FOR ANY UNIT
Ys <- genouts(Y,Z,ate=0)          

# generate the sampling distribution  
# based on the schedule of potential outcomes implied by the null hypothesis (Ys)
distout <- gendist(Ys,perms,prob=probs)  

#--------------------------------------------------------------
# P- value assuming ATE=0 for every unit
#--------------------------------------------------------------

# estimated ATE
ate                                
# one-tailed comparison used to calculate p-value (greater than)
sum(distout >= ate)
# two-tailed comparison used to calculate p-value
sum(abs(distout) >= abs(ate))       

# display p-values, 95% confidence interval, standard error under the null
# and graph the sampling distribution under the null
dispdist(distout,ate)               

#--------------------------------------------------------------
# estimation of confidence intervals assuming ATE=estimated ATE
#--------------------------------------------------------------
# create potential outcomes UNDER THE ASSUMPTION THAT ATE=ESTIMATED ATE
Ys <- genouts(Y,Z,ate=ate)            

# generate the sampling distribution 
# based on the schedule of potential outcomes implied by the null hypothesis
distout <- gendist(Ys,perms,prob=probs)  # centered on ATE now

# display p-values, 95% confidence interval, standard error under the null, 
# and graph the sampling distribution under the null
dispdist(distout,ate)    

# the red dashed line is estimated ATE

