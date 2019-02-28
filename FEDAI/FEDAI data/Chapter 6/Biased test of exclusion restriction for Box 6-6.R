# program to create the box for chapter 6 based on simulation 1

library(foreign)

simdata <- read.dta("Chapter 6_Generic Downstream Results Dataset.dta")

Z <- rep(simdata$z,simdata$wt)
D <- rep(simdata$d,simdata$wt)
Y <- rep(simdata$y,simdata$wt)

# show biased regression of Y on D and Z
(summary(lm(Y ~ D+Z)))
