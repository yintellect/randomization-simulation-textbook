rm(list=ls())

library(foreign)
library(ri)
set.seed(1234567)
teacherdata <- read.dta("/Users/donaldgreen/Dropbox/Field Experimentation Book/Statistical Routines for examples/Covariates and blocking/teachers simulated data for chapter 4.dta")


Ys <- data.frame(Y1=teacherdata$y1,Y0=teacherdata$y0)

randfun.biased <- function(){
	Zcand <- sample(c(rep(1,20),rep(0,20)))
	while (mean(teacherdata$x[Zcand==1]) < mean(teacherdata$x[Zcand==0])) Zcand <- sample(Zcand)
	return(Z=Zcand)
}

perms <- genperms.custom(numiter=200000,randfun.biased)  # customize the permutation matrix given restrictions

# illustration of biased difference-in-means
falsedist.no.adj <- gendist(Ys,perms,prob=rep(.5,nrow(Ys)))  # override weights (to induce a problem)
summary(falsedist.no.adj)

# nearly unbiased regression adjustment without using IPW
falsedist.adj <- gendist(Ys,perms,X=teacherdata$x,prob=rep(.5,nrow(Ys)))
summary(falsedist.adj)

# another way to address bias: inverse probability weights
truedist.no.adj <- gendist(Ys,perms)  # IPW estimates--asymptotically unbiased when sum(weights) in each group varies
summary(truedist.no.adj)

# illustrate the relationship between the prob of treatment and the covariate
plot(teacherdata$x,genprob(perms))

# use regression adjustment and IPW
truedist.adj <- gendist(Ys,perms,X=teacherdata$x)   # covariate adjusted IPW
summary(truedist.adj)