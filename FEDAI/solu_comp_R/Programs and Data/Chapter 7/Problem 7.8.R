# Exercise 7.8 (Trimming Bounds)

rm(list=ls())       # clear objects in memory

# Reproduce dataset
Z <- c(rep(0, 106), rep(1, 111)) #W =0, H=1
y <- c(rep(NA, 28), rep(1, 50), rep(0, 28), 
       rep(NA, 17), rep(1,68), rep(0, 26))

# Calculate rates of missingness by treatment group
prob.na.treated <- sum(is.na(y[Z==1]))/length(y[Z==1])
prob.na.control <- sum(is.na(y[Z==0]))/length(y[Z==0])

# Calculate Q
Q <- ((1 - prob.na.treated) - (1- prob.na.control))/(1 - prob.na.treated) 

# Sort the treatment group outcomes
Y.Z1 <- sort(y[Z==1])

# Select from the lowest up to the 1-Qth percentile
Y.Z1.low <- Y.Z1[1:ceiling(length(Y.Z1)*(1-Q))]

# Select from the highest down to the Qth percentile
Y.Z1.high <- Y.Z1[ceiling(length(Y.Z1)*Q):length(Y.Z1)]

# Calculate trimming bounds
trim <- c(mean(Y.Z1.low) - mean(y[Z==0], na.rm=TRUE), 
          mean(Y.Z1.high) - mean(y[Z==0], na.rm=TRUE))

trim