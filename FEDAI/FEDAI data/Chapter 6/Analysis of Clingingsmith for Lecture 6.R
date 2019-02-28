# Program to create the regression boxes for Chapter 6 using NYC Debates data  Mullainathan et al 2010

#Clear any previous work
rm(list=ls(all=TRUE))

# note first must set file directory where data file is stored

# example for mac: setwd("/Users/name/Documents/Data/")
# example for windows: setwd("C:/Documents/Data/") or setwd("C:\\Documents\\Data\\")
# if not sure of working directory, type: getwd()

#Load Relevant packages (Note that when you download sem, you will also have to download matrixcalc.)

library(foreign)
library(AER)
library(sandwich)
library(gmodels)    # package creates crosstabs

# Import Data
hajjdata <- read.dta("Chapter 6_Clingingsmith, Khwaja, and Kremer (2009) Dataset.dta")

attach(hajjdata)
hajjdata <- as.data.frame(hajjdata)

head(hajjdata)

# Rename Variables
Z <- hajjdata$success
D <- hajjdata$hajj2006
Y <- hajjdata$views


# View ITT_D as a crosstab
CrossTable(D,Z,prop.r=F,prop.t=F,prop.chisq=F,format="SPSS")

# Regression Estimate of the ITT_D
coef(summary(lm(D ~ Z)))
# robust SEs
ittD_fit <- lm(D ~ Z)
coeftest(ittD_fit,vcovHC(ittD_fit))

# Compare means to get ITT
mean(Y[Z==0])
mean(Y[Z==1])
mean(Y[Z==1])-mean(Y[Z==0])  # ITT

# Regression Estimate of the ITT
coef(summary(lm(Y ~ Z)))
# robust SEs
itt_fit <- lm(Y ~ Z)
coeftest(itt_fit,vcovHC(itt_fit))


# 2SLS Regression Estimate of CACE 
coef(summary(ivreg(Y ~ D,~Z)))
# robust SEs
cace_fit <- ivreg(Y ~ D,~Z)
coeftest(cace_fit,vcovHC(cace_fit))


