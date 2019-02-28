library(foreign)

teacherout <- read.dta("/Users/donaldgreen/Dropbox/Field Experimentation Book/Statistical Routines for examples/teacheroutput.dta")

attach(teacherout)

par(family="Gill Sans MT",font.main=1)
layout(matrix(c(1,2),2,1,byrow=TRUE))

hist(diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.25),main="Sampling Distributions",xlab="Difference-in-Means")
lines(density(diffinmean))
hist(diffinchangemeans,xlim=c(-10,20),freq=FALSE,ylim=c(0,.25),main=NULL,xlab="Difference-in-Differences")
lines(density(diffinchangemeans))

detach(teacherout)

#########

teacherout <- read.dta("/Users/donaldgreen/Dropbox/Field Experimentation Book/Statistical Routines for examples/teacheroutput.dta")

par(family="Gill Sans MT",font.main=1)
layout(matrix(c(1,2,3),3,1,byrow=TRUE))

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main="Sampling Distributions",xlab="Complete Randomization")
lines(density(teacherout$diffinmean))

teacherout <- read.dta("/Users/donaldgreen/Dropbox/Field Experimentation Book/Statistical Routines for examples/teacheroutputblock.dta")

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main=NULL,xlab="Blocked Randomization (Strong Predictor)")
lines(density(teacherout$diffinmean))

teacherout <- read.dta("/Users/donaldgreen/Dropbox/Field Experimentation Book/Statistical Routines for examples/teacheroutputblockweak.dta")

hist(teacherout$diffinmean,xlim=c(-10,20),freq=FALSE,ylim=c(0,.30),main=NULL,xlab="Blocked Randomization (Weak Predictor)")
lines(density(teacherout$diffinmean))

