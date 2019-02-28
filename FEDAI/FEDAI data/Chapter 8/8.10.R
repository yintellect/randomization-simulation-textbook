library(foreign)
library(ri)
rm(list = ls())
hough <- read.csv("Chapter 8_Leslie Hough self-experiment data.csv")

Y <- hough$tetris
Z <- hough$run
N <- length(Z)
# exclude day 1 from analysis
Zlag <- c(NA,Z[1:N-1])
Ylag <- c(NA,Y[1:N-1])
# simple random assignment based on coin flips
randfun <- function() rbinom(N,1,.5)
numiter <- 10000
set.seed(343)
# random assignment follows the custom function "randfun" perms <- genperms.custom(numiter = numiter, randfun = randfun)
## note on missing data: the default for LM is NA.omit=TRUE
## This default eliminates the first lag, and the two days with missing outcomes
### This code performs the estimation for parts b, c, and d
fit1 <- lm(Y ~ Z)$coefficients["Z"]
fit2 <- summary(lm(Y ~ Z + Zlag))$fstatistic[1]
fit3 <- lm(Ylag ~ Z)$coefficients["Z"]
fit4 <- lm(hough$energy ~ Z)$coefficients["Z"]
fit5 <- lm(hough$gre ~ Z)$coefficients["Z"]
