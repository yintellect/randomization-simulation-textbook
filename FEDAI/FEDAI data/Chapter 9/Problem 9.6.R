# Exercise 9.6 -- Tipping, Happyfaces, and Waitstaff Sex

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(123908)   # random number seed, so that results are reproducible

setwd("~/16Spring/4368 Experimental/4.FEDAI replication/Chapter 9")
library(foreign)    # package allows R to read Stata datasets

# Data are from Rind, Bruce and Prashant Bordia. 1996. “Effect on Restaurant Tipping of Male and Female Servers Drawing a Happy, Smiling Face on the Backs of Customers’ Checks.” Journal of Applied Social Psychology. 26: 218-225.

rindb <- read.dta("Rind_Bordia_JASP_1996.dta")

head(rindb)  # show a snippet of the data

Z <- as.integer(rindb$happyface) - 1   # generate a treatment indicator
Y <- rindb$tip

probs <- genprobexact(Z)

ate <- estate(Y,Z,prob=probs)

numiter <- 100000

perms <- genperms(Z,maxiter=numiter)

Ys <- genouts(Y,Z,ate=ate)            # generate a schedule of potential outcomes under the assumption that the treatment effect equals the estimated ATE for all subjects


testvar <- var(Y[Z==1]) - var(Y[Z==0])   
# compare variances in treatment and control groups
vardist <- rep(NA,numiter)               

# generate the sampling distribution of the difference in variances

for (i in 1:numiter) 
        vardist[i] <- var(Y[perms[,i]==1]) - var(Y[perms[,i]==0])

# (a)

mean(vardist >= testvar)           # p-value for var(Y1)>Var(Y0)
mean(abs(vardist) >= abs(testvar)) # p-value for var(Y1)<>Var(Y0)

Ys # schedule of Potential Outcomes

# (b)

female <- as.integer(rindb$female) - 1   # generate a treatment indicators of waitstaff sex

lmmodelint <- lm(Y~Z*female)      # regression with interaction between happyface and waitstaff sex

summary(lmmodelint)

# (c)

lmmodel <- lm(Y~Z+female)    # regression model without interaction

Y0 <- Y - lmmodel$coefficients["Z"] * Z
Y1 <- Y + lmmodel$coefficients["Z"] * (1 - Z)

f.obs <- lmtest::waldtest(lmmodelint, lmmodel)$F[2]

f.sims <- rep(NA,numiter)
for (i in 1:numiter) {
        Z.sim <- perms[,i]
        Y.sim <- Y1 * Z.sim + Y0 * (1 - Z.sim)
        # regressions based on two nested models: with and without interaction
        lmmodelint.sim <- lm(Y.sim ~ Z.sim + female + female * Z.sim)
        lmmodel.sim <- lm(Y.sim ~ Z.sim + female)
        # calculate the F-statistic by comparing two nested models
        f.sims[i] <- lmtest::waldtest(lmmodelint.sim, lmmodel.sim)$F[2]
}
# calculate the p-value by comparing the observed F-statistic
# to the F-statistic under the null of constant & additive effects 
mean(f.sims >= f.obs)


round(sample(f.sims, replace = FALSE),3)






# use estimated coefficients from base model to impute potential outcomes
Y00 <- Y - lmmodel$coefficients["Z"]*Z - lmmodel$coefficients["female"]*female
Y10 <- Y00 + lmmodel$coefficients["Z"]
Y01 <- Y00 + lmmodel$coefficients["female"]
Y11 <- Y00 + lmmodel$coefficients["Z"] + lmmodel$coefficients["female"] 

Zstat <- Z*10 + female   
# vector of all combinations of treatments 
# that will later be permuted in order to simulate possible combinations

# calculate the F-statistic by comparing two nested models
ftest <- lmtest::waldtest(lmmodelint,lmmodel)$F[2]

flist <- rep(NA,numiter)

N <- length(Z)

for (i in 1:numiter) {
	Zstatri <- sample(Zstat)   # sample from the set of observed set of single or combined treatments
	
	# realized values of Y reflect single or compound treatments and 
	# the potential outcomes that they reveal
	Yri <- Y00
	Yri[Zstatri == 10] <- Y10[Zstatri == 10]
	Yri[Zstatri == 1] <- Y01[Zstatri == 1]
	Yri[Zstatri == 11] <- Y11[Zstatri == 11]
	
	# adjust the simulated variables Z and female to reflect the assignments above
	Zri <- femaleri <- rep(0,N)
	Zri[Zstatri == 10 | Zstatri == 11] <- 1
	femaleri[Zstatri == 1 | Zstatri == 11] <- 1
	
	# regressions based on two nested models: with and without interaction
	lmmodelintri <- lm(Y~Zri*femaleri)
	lmmodelri <- lm(Y~Zri+femaleri)
	# calculate the F-statistic by comparing two nested models
	flist[i] <- lmtest::waldtest(lmmodelintri,lmmodelri)$F[2]	
}

mean(f.sims  >= ftest)   # calculate the p-value by comparing the observed F-statistic to the F-statistic under the null of constant & additive effects
