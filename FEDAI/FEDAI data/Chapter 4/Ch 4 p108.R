#   -----------------------------------------------------------------------
#   Control the covariates for EX POST (P108)
#   (Example: Teachers' incentive, Muralidharan & Sundararaman 2011)
#   -----------------------------------------------------------------------


#   -----------------------------------------------------------------------
rm(list=ls())
library(foreign)
library(ri)
set.seed(1234567)
setwd("~/Desktop/2016/4368 Experimental/5.FEDAI replication/Chapter 4")
teacherdata <- read.dta("Teachers data for Table 4-1.dta")



#   -----------------------------------------------------------------------
# Observed outcome
Ys <- data.frame(Y1=teacherdata$y1,Y0=teacherdata$y0)

#   Theory statement   ----------------------------------------------------
# EX post: We do not expect the difference-in-mean procedure will generate unbaises estimation

# Reason: restrict our attention only those randomizations in which 
# the average pre-test socres of treatment group exceeded that of control group

# Conditional on observing an imbalance on a prognostic variable 
# that favors the treatment group, the result estimated ATE > true ATE


#   Example display (Teachers' Incentive table 4.1)  ----------------------

# Now we create a function that only generate a subset(confitional) pemutations that 
# the average pre-test socres of treatment group exceeded that of control group 
randfun.biased <- function(){
	Zcand <- sample(c(rep(1,20),rep(0,20)))
	while (mean(teacherdata$x[Zcand==1]) < mean(teacherdata$x[Zcand==0])) 
	        Zcand <- sample(Zcand)
	return(Z=Zcand)
}

# The permutations that pre-test scores favor the treatment group
randfun.biased() # each is one of the subset permutation


#   ---------------------------------------------------------------------------------
#   P.108  Ex post: we might now expect our estimated ATE to be a higher-than-aerage
#                   draw from th esampling distribution
#   ---------------------------------------------------------------------------------

# use this subset permutations to do randomize inference
# customize the permutation matrix given restrictions (just add our subset function)
perms <- genperms.custom(numiter=200000,randfun.biased) 


# illustration of biased difference-in-means

# generate 200000 times simulation in the conditional(baised) permutations

# gendist(generate sampling distribution for randomization inference)
# Parameter: gendist(Ys(compelete random schedule), 
#                    perms(possible permutations),
#                    prob(probabilities,should specify block or clusters))

# this is false distribution without covariate adjustment
falsedist.no.adj <- gendist(Ys,perms,prob=rep(.5,nrow(Ys)))

# Use hisogram to displayfalse distribution
hist(falsedist.no.adj,breaks=100)

# Use summary to check the average estimate ATE 
#                    conditional on pre-test score favoring the treatment group
summary(falsedist.no.adj)
abline(v=7.67,lty=3,col="red",lwd=3)

# The true ATE
mean(teacherdata$y1)-mean(teacherdata$y0)
abline(v=4,lty=3,col="blue",lwd=3)

# Summary: Conditional on observing an imbalance on a prognostic variable that
#          favors treatment group, Estimated ATE > true ATE


#   ---------------------------------------------------------------------------------
#   P.109  Solution for Ex post: 
#   if covariate is imbalance due to random assignment, 
#                   controlling for this imbalance produces unbiased estimates.
#   ---------------------------------------------------------------------------------

#   ---------------------------------------------------------------------------------
#   Control for pre-test scores (Covariates) without using IPW -----------

# Take covariates(pre-test score) into consideration 
# "Control" means we compare treatment and control outcomes 
#                              within subjects have the same pre-test score
# Since the comparasion are not differ with regard to the covariates
#                             we expect our estimate is unbiased


# now we add a parameter in gendist()-- X= teacherdata$x
falsedist.adj <- gendist(Ys,perms,X=teacherdata$x,prob=rep(.5,nrow(Ys)))
# prob=rep(.5,nrow(Ys) means for every subject the probability of 
# receiving treatment is 0.5, there is no inverse probability weights


# now check the average estimate ATE with covariate adjustment
summary(falsedist.adj) # pretty close to 4(true ATE)

# Use hisogram to displayfalse distribution
hist(falsedist.adj,breaks=100)
# draw the estimate ATE with covariate adjustment
abline(v=3.996 ,lty=3,col="red",lwd=3)

# draw the true ATE
abline(v=4,lty=3,col="blue",lwd=3)

# the estimate ATE with covariate adjustment is nearly overlap with true ATE!
# Summary: if covariate is imbanlance due to random assignment, 
#          controlling for this imbanlance(covariates) produce unbiases estimates.

#   ---------------------------------------------------------------------------------
#   Using inverse probability weights (IPW) --------------------------------

# First we generate true distribution 
# (without specifing probs, algorithm assumes probability implied by the permutation matrix)
truedist.no.adj <- gendist(Ys,perms)  
# take a look at the true distribution
hist(truedist.no.adj,breaks=100)

summary(truedist.no.adj)

# The probability of each observation would be assigned to its actual experiment group 
genprob(perms) 

# illustrate the relationship between the prob of treatment and the covariate
plot(teacherdata$x,genprob(perms))

# we can see that subjects with higher pre-test score 
#                           are more likely to receive treatment


# -----------------------------------------------------------------------
# ????????????
# use regression adjustment and IPW
truedist.adj <- gendist(Ys,perms,X=teacherdata$x)   # covariate adjusted IPW
# prob= NULL assumes probability implied by the permutation matrix
summary(truedist.adj)
