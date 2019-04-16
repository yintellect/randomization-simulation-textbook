# Exercise 8.10 (self-experiment on the effects of running)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible

# Set your working directory
# setwd()

# Data are from Hough, Leslie. 2010. 
# “Experimenting with an N of 1.” Yale University Working Paper.

hough <- read.csv("Problem 8.10 Hough.csv", stringsAsFactors = FALSE)

Y <- hough$tetris
Z <- hough$run
N <- length(Z)

# exclude day 1 from analysis
Zlag <- c(NA,Z[2:N-1])
Ylag <- c(NA,Y[2:N-1])

# simple random assignment based on coin flips
randfun <- function() rbinom(N,1,.5)    
numiter <- 10000
set.seed(343)
# random assignment follows the custom function "randfun"
perms <- genperms.custom(numiter=numiter,randfun=randfun)  

## note on missing data: the default for LM is NA.omit=TRUE
## This default eliminates the first lag, and the two days with missing outcomes

### This code performs the estimation for parts b, c, and d

fit1 <- lm(Y~Z)$coefficients["Z"]             # regress Y on current Z
fit2 <- summary(lm(Y~Z+Zlag))$fstatistic[1]   # regress Y on current and lagged Z
fit3 <- lm(Ylag~Z)$coefficients["Z"]          # placebo fit: regress lagged Y on Z
fit4 <- lm(hough$energy~Z)$coefficients["Z"]  # consider current Z's effects on energy
fit5 <- lm(hough$gre~Z)$coefficients["Z"]     # consider current Z's effects on GRE

# initialize the five vectors of results
dist1 <- dist2 <- dist3 <- dist4 <- dist5 <- rep(NA,numiter)

for (i in 1:numiter) {
  Zri <- perms[,i]
  Zlagri <- c(NA,Zri[2:N-1]) # exclude day 1 from analysis
  
  dist1[i] <- lm(Y~Zri)$coefficients["Zri"]
  dist2[i] <- summary(lm(Y~Zri+Zlagri))$fstatistic[1]
  dist3[i] <- lm(Ylag~Zri)$coefficients["Zri"]
  dist4[i] <- lm(hough$energy~Zri)$coefficients["Zri"]
  dist5[i] <- lm(hough$gre~Zri)$coefficients["Zri"]
}

mean(dist1 >= fit1)      # one-tailed p-value: does running increase Tetris scores
mean(dist2 >= fit2)      # one-tailed p-value: does running increase Tetris scores
mean(abs(dist3) >= abs(fit3))  # two-tailed p-value: placebo fit
mean(dist4 >= fit4)      # one-tailed p-value: does running improve energy
mean(dist5 >= fit5)      # one-tailed p-value: does running improve GRE

	