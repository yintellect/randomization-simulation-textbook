rm(list = ls())
library(foreign)

#   -----------------------------------------------------------------------
#   Figure 4.1
#   -----------------------------------------------------------------------


teacherout <- read.dta("GerberGreenBook_Chapter4_Simulated_TeacherOutput.dta")
attach(teacherout)

par(family="Palatino",font.main=1)
layout(matrix(c(1,2),2,1,byrow=TRUE))

hist(diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.25),main="Sampling Distributions",xlab="Difference-in-Means")
lines(density(diffinmean))

hist(diffinchangemeans,xlim=c(-10,20),freq=FALSE,ylim=c(0,.25),main=NULL,xlab="Difference-in-Differences")
lines(density(diffinchangemeans))

detach(teacherout)

#   -----------------------------------------------------------------------
#   Figure 4.2
#   -----------------------------------------------------------------------

teacherout <- read.dta("GerberGreenBook_Chapter4_Simulated_TeacherOutput.dta")

par(family="Palatino",font.main=1)
layout(matrix(c(1,2,3),3,1,byrow=TRUE))

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main="Sampling Distributions",xlab="Simple Randomization")
lines(density(teacherout$diffinmean))

teacherout <- read.dta("GerberGreenBook_Chapter4_Simulated_Teacheroutputblock.dta")

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main=NULL,xlab="Blocked Randomization (Strong Predictor)")
lines(density(teacherout$diffinmean))

teacherout <- read.dta("GerberGreenBook_Chapter4_Simulated_Teacheroutputblockweak.dta")

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main=NULL,xlab="Blocked Randomization (Weak Predictor)")
lines(density(teacherout$diffinmean))

