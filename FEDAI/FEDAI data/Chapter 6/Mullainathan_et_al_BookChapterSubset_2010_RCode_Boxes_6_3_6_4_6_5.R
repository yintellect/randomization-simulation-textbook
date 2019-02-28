# Program to create the regression boxes for Chapter 6 using NYC Debates data  Mullainathan et al 2010

#Clear any previous work
rm(list=ls(all=TRUE))

library(foreign)
library(AER)

# Import Data from Website
teacherdata <- read.csv("http://hdl.handle.net/10079/kh189dd")

teacherdata <- as.data.frame(teacherdata)

# Rename Variables
Z <- teacherdata$watch
D <- teacherdata$watchdps
Y <- teacherdata$ochange

# Box 6.3: Regression Estimate of the ITT_D
out.lm <- lm(D ~ Z)
summary(out.lm)

ITTd <- coef(out.lm)[2]

# Box 6.4: Regression Estimate of the ITT
out.lm <- lm(Y ~ Z)
summary(out.lm)

ITT <- coef(out.lm)[2]

# Box 6.5: 2SLS Regression Estimate of CACE 
# format is tsls(Y~D+X,~Z+X)

out.ivreg <- ivreg(Y~D|Z)
summary(out.ivreg)

ITT/ITTd ## Check value
