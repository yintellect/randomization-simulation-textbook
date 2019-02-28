# Program to create Box 6.6 based on simulation 1 of downstream data experiment 

#Clear any previous work
rm(list=ls(all=TRUE))

#Load Relevant packages
library(foreign)

# Read in Data
simdata <- read.dta("http://hdl.handle.net/10079/q573nh7")

Z <- rep(simdata$z,simdata$wt)
D <- rep(simdata$d,simdata$wt)
Y <- rep(simdata$y,simdata$wt)

# show biased regression of Y on D and Z
(summary(lm(Y ~ D+Z)))
