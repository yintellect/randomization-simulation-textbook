# Exercise 8.10 (self-experiment on the effects of running)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(123)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
setwd("~/16Spring/4368 Experimental/4.FEDAI replication/Chapter 8")
# Data are from Hough, Leslie. 2010. “Experimenting with an N of 1.” Yale University Working Paper.

hough <- read.dta("Chapter 8_Leslie Hough self-experiment data.dta")

# Part (b)

Y <- hough$tetris
Z <- hough$run

N <- length(Z)

# lagged time variable 
Zlag <- c(NA,Z[2:N-1]) # exclude day 1 from analysis
Ylag <- c(NA,Y[2:N-1])

randfun <- function() rbinom(N,1,.5)    # simple random assignment based on coin flips
numiter <- 10000
perms <- genperms.custom(numiter=numiter,randfun=randfun)  # random assignment follows the custom function "randfun"

## note on missing data: the default for LM is NA.omit=TRUE
## This default eliminates the first lagged observation and the two days with missing outcomes

test1 <- lm(Y~Z)$coefficients["Z"]             # regress Y on current Z
test2 <- summary(lm(Y~Z+Zlag))$fstatistic[1]   # regress Y on current and lagged Z (test no-persistence)
test3 <- lm(Ylag~Z)$coefficients["Z"]          # placebo test: regress lagged Y on Z (test no-anticipation)
test4 <- lm(hough$energy~Z)$coefficients["Z"]  # consider current Z's effects on energy
test5 <- lm(hough$gre~Z)$coefficients["Z"]     # consider current Z's effects on GRE

# initialize the five vectors of results (all 5 at the same time!)
testdist1 <- testdist2 <- testdist3 <- testdist4 <- testdist5 <- rep(NA,numiter)

for (i in 1:numiter) {
	
	Zri <- perms[,i]
	Zlagri <- c(NA,Zri[2:N-1]) # exclude day 1 from analysis

testdist1[i] <- lm(Y~Zri)$coefficients["Zri"]
testdist2[i] <- summary(lm(Y~Zri+Zlagri))$fstatistic[1]
testdist3[i] <- lm(Ylag~Zri)$coefficients["Zri"]
testdist4[i] <- lm(hough$energy~Zri)$coefficients["Zri"]
testdist5[i] <- lm(hough$gre~Zri)$coefficients["Zri"]
	
	}
	
mean(testdist1 >= test1)      # one-tailed p-value: does running increase Tetris scores
mean(testdist2 >= test2)      # one-tailed p-value: does running increase Tetris scores (test no-persistence)
mean(abs(testdist3) >= abs(test3))  # two-tailed p-value: placebo test (test no-anticipation)
mean(testdist4 >= test4)      # one-tailed p-value: does running improve energy
mean(testdist5 >= test5)      # one-tailed p-value: does running improve GRE
	
	