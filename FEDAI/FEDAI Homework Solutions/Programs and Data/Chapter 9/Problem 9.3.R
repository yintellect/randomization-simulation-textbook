# Exercise 9.3

rm(list=ls())       # clear objects in memory

# Set your working directory
# setwd("")

# Recreate Dataset
block <- c(rep("A", 4), rep("B", 4))
Y0 <- c(0,1,1,2,2,3,4,4)
Y1 <- c(2,5,3,1,3,3,9,7)

# function for calculating population covariances
cov.pop <- function(x,y){sum((x-mean(x))*(y-mean(y)))/(length(x))}

# Ignoring blocks
Y1.lowtohigh <- sort(Y1)
Y1.hightolow <- sort(Y1, decreasing=TRUE)

cov.min <- cov.pop(Y0, Y1.hightolow)
cov.max <- cov.pop(Y0, Y1.lowtohigh)
cov.min
cov.max

# Including blocks
Y1.lowtohigh.block <- c(sort(Y1[block=="A"]), sort(Y1[block=="B"]))
Y1.hightolow.block <- c(sort(Y1[block=="A"],decreasing=TRUE), 
                        sort(Y1[block=="B"],decreasing=TRUE))
cov.min.block <- cov.pop(Y0, Y1.hightolow.block)
cov.max.block <- cov.pop(Y0, Y1.lowtohigh.block)
cov.min.block
cov.max.block