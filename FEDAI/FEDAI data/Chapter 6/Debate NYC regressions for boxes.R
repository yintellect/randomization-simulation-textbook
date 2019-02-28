# Program to create the regression boxes for Chapter 6 using NYC Debates data  Mullainathan et al 2010

#Clear any previous work
rm(list=ls(all=TRUE))

# note first must set file directory where data file is stored

# example for mac: setwd("/Users/name/Documents/Data/")
# example for windows: setwd("C:/Documents/Data/") or setwd("C:\\Documents\\Data\\")
# if not sure of working directory, type: getwd()

#Load Relevant packages (Note that when you download sem, you will also have to download matrixcalc.)

library(foreign)
library(sem)

# Import Data
teacherdata <- read.csv("/Users/alissastollwerk/Documents/Columbia/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 6/Mullainathan_et_al_BookChapterSubset_2010.csv")

attach(teacherdata)
teacherdata <- as.data.frame(teacherdata)

# Rename Variables
Z <- teacherdata$watch
D <- teacherdata$watchdps
Y <- teacherdata$ochange

# Box 6.3: Regression Estimate of the ITT_D
out.lm <- lm(D ~ Z)
summary(out.lm)

# Box 6.4: Regression Estimate of the ITT
out.lm <- lm(Y ~ Z)
summary(out.lm)

# Box 6.5: 2SLS Regression Estimate of CACE 
# format is tsls(Y~D+X,~Z+X)

out.tsls <- tsls(Y ~ D, ~ Z)
summary(out.tsls)