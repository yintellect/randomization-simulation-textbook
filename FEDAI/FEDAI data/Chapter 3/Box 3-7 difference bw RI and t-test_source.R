# Guide: Use the yourdata.csv template for your data. Then run all code, all output will be in graphic display. Red dotted line indicates actual estimated ATE.
# For problems or questions with the code, contact peter.aronow@yale.edu

# Data Import -- change the working directory to fit your directory setup
setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 3/")
dataf <- read.csv("donations.csv") # enter your file name here

# Advanced Options

numiter <- 100000 # number of RI iterations. Use more for greater precision, fewer for greater speed.
set.seed(1337) # set random number seed (so that results can be replicated)

# Compute RI Distribution

Y <- dataf$Y
Z <- dataf$Z

denom <- var(Z)
tau <- cov(Y,Z)/denom

tauRI <- rep(NA,numiter)

for (i in 1:numiter) {

Zri <- sample(Z)
tauRI[i] <- cov(Y,Zri)/denom

if (i %% round(numiter/10) == 0) cat("Iteration",i,"of",numiter,"\n")
}

# RI distribution output

hist(tauRI,freq=TRUE,xlab="Estimated ATE",main=paste("Distribution of the Estimated ATE\n under the Sharp Null Hypothesis of No Treatment Effect\np = ",round(sum(tauRI >= tau)/numiter,3),", SE = ",round(sd(tauRI),3),sep=""),breaks=numiter,lwd=1)
lines(x=c(tau,tau),y=c(-1000,numiter*2),lwd=2,col="red",lty=2)

pvalue <- round(sum(tauRI >= tau)/numiter,3)
pvalue

# compare with traditional t-tests

t.test(Y~Z,var.equal=FALSE,alternative= "less")
t.test(Y~Z,var.equal=TRUE,alternative= "less")

# compare with regression

out.lm <- lm(Y ~ Z)
summary(out.lm)