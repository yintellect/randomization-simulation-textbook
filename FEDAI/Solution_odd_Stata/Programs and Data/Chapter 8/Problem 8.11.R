# Exercise 8.11 (Stepped wedge)

rm(list=ls())       # clear objects in memory

# Set your working directory
# setwd("")

#### Problem 8.11a ####

# Recreate dataset
week <- c(rep("2", 8), rep("3", 8))
prob00 <- c(rep(0.5, 8), rep(0.25, 8))
prob01 <- c(rep(0.25, 8), rep(0.25, 8))
prob11 <- c(rep(0.25, 8), rep(0.50, 8))
Y <- c(9,5,2,3,3,8,3,1,
       4,7,10,10,3,10,4,3)
Z <- c("11", "00", "01", "00", "00", "11", "00", "01",
       "11", "01", "11", "01", "00", "11", "00", "11")

# Estimate E[Y_{01}-Y_{00}]
mean01 <- weighted.mean(Y[Z=="01"], w=1/prob01[Z=="01"])
mean00 <- weighted.mean(Y[Z=="00"], w=1/prob01[Z=="00"])
ate01_00 <- mean01 - mean00
ate01_00

#### Problem 8.11b ####

# Estimate E[Y_{11}-Y_{00}]
mean11 <- weighted.mean(Y[Z=="11" & week=="2"], w=1/prob01[Z=="01" & week=="2"])
mean00 <- weighted.mean(Y[Z=="00" & week=="2"], w=1/prob01[Z=="00" & week=="2"])
ate11_00 <- mean11 - mean00
ate11_00