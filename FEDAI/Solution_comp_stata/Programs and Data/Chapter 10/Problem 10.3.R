# Exercise 10.3

rm(list=ls())       # clear objects in memory
library(ri)

#### Problem 10.3j ####

# Recrete Potential Outcomes
Z <- c(0,0,0,0,0,0,1,1,1,1,1,1)

Y0M0 = c(0,0,0,0,0,0,1,1,1,0,0,0)
Y1M0 = c(0,0,0,1,1,1,0,0,0,1,1,1)
Y0M1 = c(0,0,0,0,0,0,1,1,1,1,1,1)
Y1M1 = c(0,0,0,1,1,1,1,1,1,1,1,1)
M0 = c(0,0,1,0,0,1,0,0,1,0,0,1)
M1 = c(0,1,1,0,1,1,0,1,1,0,1,1)

# verify column averages
mean(Y0M0)
mean(Y1M0)
mean(Y0M1)
mean(Y1M1)

# simulate all possible random assignments
perms <- genperms(Z)

# stores estimates from equation 10.3
coefmat <- matrix(NA,ncol(perms),3)  
# stores estimates from equation 10.2
tcoefmat <- matrix(NA,ncol(perms),2) 
# stores estimates from equation 10.1
mcoefmat <- matrix(NA,ncol(perms),2) 

for (i in 1:ncol(perms)) {
  Zri <- perms[,i]
  M <- M0*(1-Zri) + M1*Zri
  Y <- Y0M0*(1-Zri)*(1-M) + Y1M0*(Zri)*(1-M) + 
    Y0M1*(1-Zri)*(M) + Y1M1*(Zri)*(M)
  coefmat[i,] <- lm(Y~M+Zri)$coefficients
  tcoefmat[i,] <- lm(Y~Zri)$coefficients
  mcoefmat[i,] <- lm(M~Zri)$coefficients
}

# results omit instances of perfect colinearity between M and Z
# report the avg coefficients from a regression of Y on M and Z 
colMeans(na.omit(coefmat))  
# report the avg coefficients from a regression of Y on Z 
colMeans(na.omit(tcoefmat)) 
# report the avg coefficients from a regression of M on Z 
colMeans(na.omit(mcoefmat)) 

#### Problem 10.3k ####

# Estimates of a, from simulation above
as <- mcoefmat[,2]
# Estimates of b, from simulation above
bs <- coefmat[,3]
# This average is very nearly 1/12.
mean(as*bs, na.rm = TRUE)
