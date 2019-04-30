# Exercise 7.6 (Voucher experiment)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
library(foreign)    # package allows R to read Stata datasets
set.seed(1234567)   # random number seed, so that results are reproducible

# Set your working directory
# setwd("")

# Data are from Angrist, Bettinger, and Kremer 2006

angrist <- read.dta("Problem 7.6 Angrist.dta")

# Subset
angrist_s <- subset(angrist, age>=9 & age <= 25 & checkid==1 )

angrist_s <- within(angrist_s,{
  read[is.na(read)] <- 0
  sex <- sex_name
  observed <- 1 - (read == 0)
  
  # Predicted propensity scores from logistic model
  probobs <- glm(observed~(vouch0*sex)+(vouch0*phone)+(vouch0*age),
                 family=binomial(link="logit"))$fitted
  weights <- 1/probobs
})

# Verify that all probabilities are less than one and greater than zero
with(angrist_s, {
  rbind(summary(probobs[vouch0==0]),
        summary(probobs[vouch0==1]))
})

# Coefficients for unweighted regression (restricting analysis to observed subjects)
lm(read~vouch0, data=subset(angrist_s, observed==1))$coefficients

# Coefficients for IPW regression (restricting analysis to observed subjects)
lm(read~vouch0, weights=weights, data=subset(angrist_s, observed==1))$coefficients