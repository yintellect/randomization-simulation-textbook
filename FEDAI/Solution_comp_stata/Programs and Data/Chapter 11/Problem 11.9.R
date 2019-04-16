# Exercise 11.9 Bed Nets

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
library(arm)        # for invlogit()
library(dplyr)      # for data manipulation

# Set your working directory
# setwd("")


dupas <- read.dta("Problem 11.8 Dupas.dta")
dupas <- within(dupas,{
  purchased <- as.numeric(purchasednet =="yes")
  log_price <- log(price)
  region <- cfw_id
})



gammas <- seq(1:100)
lls <- rep(NA, length(gammas))


# Loop through possible values of gamma
for(i in 1:length(gammas)){
  dupas <- within(dupas,{
    log_price_star <- log(price+i)
  })
  fit <- glm(purchased ~ log_price_star + region, data = dupas, family = "binomial")
  lls[i] <- logLik(fit)
}
plot(gammas, lls)
abline(v = 19)

gammas[which(lls == max(lls))]