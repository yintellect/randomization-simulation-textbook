# Exercise 8.9 (Hotspots)

rm(list=ls())       # clear objects in memory

# Set your working directory
# setwd("")

# Data are from Table 8-4 and 8-5
hotspots <- read.csv("Problem 8.9 Table 8-4 and 8-5.csv", stringsAsFactors=FALSE)


#### Problem 8.9a ####

true_ate <- with(hotspots, mean(y01[prox500==0]) - mean(y00[prox500==0]))
true_ate
ate_hat <- with(hotspots, mean(y[prox500==0 & assignment==1]) - 
                  mean(y00[prox500==0 & assignment==0]))
ate_hat

#### Problem 8.9b ####

true_ate_01 <- with(hotspots, mean(y01[prox500==1]) - mean(y00[prox500==1]))
true_ate_10 <- with(hotspots, mean(y10[prox500==1]) - mean(y00[prox500==1]))
true_ate_11 <- with(hotspots, mean(y11[prox500==1]) - mean(y00[prox500==1]))
true_ate_01
true_ate_10
true_ate_11

hotspots <- within(hotspots,{
  exposure[exposure == 11] <- "11"  # Indirect and Direct Treatment
  exposure[exposure == 10] <- "10"  # Indirect Treatment
  exposure[exposure == 01] <- "01"  # Direct Treatment
  exposure[exposure == 00] <- "00"  # Control
  
  # Calculate probability of assignment to exposure condition
  Q <- NA
  Q[exposure == "11"] <- prob11[exposure == "11"]
  Q[exposure == "10"] <- prob10[exposure == "10"]
  Q[exposure == "01"] <- prob01[exposure == "01"]
  Q[exposure == "00"] <- prob00[exposure == "00"]
  
  # Generate weights
  weights <- 1/Q
})

# Estimate E[Y_{01} - Y_{00}]:
fit.01 <- lm(y ~ exposure, weights=weights, 
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "01")))

# Estimate E[Y_{10} - Y_{00}]:
fit.10 <- lm(y ~ exposure, weights=weights, 
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "10")))

# Estimate E[Y_{11} - Y_{00}]:
fit.11 <- lm(y ~ exposure, weights=weights, 
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "11")))


#### Problem 8.9c ####
rm(list=ls())
library(foreign)
hotspot_nonexp <- read.dta("Problem 8.9 Hotspots Nonexperimental.dta")

hotspot_nonexp <- within(hotspot_nonexp,{
  exposure[exposure==10] <- "10"
  exposure[exposure==0] <- "00"
  
  Q <- NA
  Q[exposure=="10"] <- prob10[exposure=="10"]
  Q[exposure=="00"] <- prob00[exposure=="00"]
  
  weights <- 1/Q
})

fit.nonexp <- lm(y ~ exposure, weights=weights, 
                 data=subset(hotspot_nonexp, prob10 > 0 & prob10 < 1))

fit.nonexp