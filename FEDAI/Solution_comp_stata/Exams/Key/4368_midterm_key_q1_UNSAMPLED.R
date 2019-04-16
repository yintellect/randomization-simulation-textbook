# ==============================================
# W4368 EXPERIMENTAL RESEARCH -- SPRING 2013
# MIDTERM EXAM KEY
# TA: Al Fang
# Date: 20 Apr 2013
# - UNSAMPLED DATASET - QUESTION 1
# ==============================================

rm(list=ls(all=T))

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/")

library(ri)
library(foreign)
library(xtable)
library(AER)


# Read in UNIs and student names to set seeds

# stinfo <- read.csv("seeds.csv", header=T)

stinfo <- as.data.frame(t(c("unsampled data","none","none")))
names(stinfo) <- c("name","uni","seed")

# Create objects to hold results
q1a.out <- matrix(NA, nrow=nrow(stinfo), ncol=2)
colnames(q1a.out) <- c("f.stat","p.value")
rownames(q1a.out) <- stinfo$name

q1j.out <- matrix(NA, nrow=nrow(stinfo), ncol=2)
colnames(q1j.out) <- c("f.stat","p.value")
rownames(q1j.out) <- stinfo$name


q1b.table.out <- array(NA, dim=c(3,6,nrow(stinfo)))
dimnames(q1b.table.out) <- list(c("treated","untreated","total"), c("mean.z1","n.z1","mean.z0","n.z0","diff","n"), stinfo$uni )

q1c.out <- matrix(NA, nrow=nrow(stinfo), ncol=13)
colnames(q1c.out) <- c("k","itt","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","itt.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q1c.out) <- stinfo$name

q1e.out <- matrix(NA, nrow=nrow(stinfo), ncol=12)
colnames(q1e.out) <- c("itt","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","itt.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q1e.out) <- stinfo$name

q1g.out <- matrix(NA, nrow=nrow(stinfo), ncol=3)
colnames(q1g.out) <- c("Pr(Complier)","Pr(Always-Taker)","Pr(Never-Taker)")
rownames(q1g.out) <- stinfo$name

q1h.out <- matrix(NA, nrow=nrow(stinfo), ncol=12)
colnames(q1h.out) <- c("cace","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","cace.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q1h.out) <- stinfo$name

q1i.out <- matrix(NA, nrow=nrow(stinfo), ncol=8)
colnames(q1i.out) <- c("itt.ev.lower","itt.ev.upper","itt.ev.lower.adj","itt.ev.upper.adj","ittHT.ev.lower","ittHT.ev.upper","ittHT.ev.lower.adj","ittHT.ev.upper.adj")
rownames(q1i.out) <- stinfo$name

# Loop over each student's UNI number
 # ============================================================================================
# ============================================================================================
# Problem 1
# ============================================================================================
# ============================================================================================

# Read in raw data, prepare person-specific dataset

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/")
data <- read.dta("q1hajj_sub.dta")

# uni <- myuni
# set.seed(uni)
# mydata <- as.data.frame(data[sample(1:nrow(data), nrow(data), replace=T),])
mydata <- data


# ==============================================
# 1A - Randomization check 
# ==============================================

numiter <- 10000

q1a.fstat <- summary(lm(success ~ age + female + literate, data=mydata))$fstatistic[1]

probs <- genprobexact(Z=mydata$success, clustvar=mydata$hhid)
perms <- genperms(Z=mydata$success, clustvar=mydata$hhid, maxiter=numiter)

 f.stat.store <- rep(NA, numiter)

 for(i in 1:numiter){
	f.stat.store[i] <- summary(lm(perms[,i] ~ mydata$age + mydata$female + mydata$literate))$fstatistic[1]
 }

 q1a.pvalue <- mean(f.stat.store >= q1a.fstat)

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/Key/")
 pdf("q1a_unsampled.pdf")
 hist(f.stat.store, breaks=numiter/100, main=paste("Unsampled Dataset","\n Q1a: Simulated distribution of F statistic under the sharp null \n Observed F statistic: ",round(q1a.fstat, digits=3),", p-value: ",round(q1a.pvalue, digits=3),sep=""), xlab="Simulated F statistic")
abline(v=q1a.fstat, col="red")
dev.off()

q1a.out[,1] <- q1a.fstat
q1a.out[,2] <- q1a.pvalue
q1a.out

# ==============================================
# 1B - Construct table/graph to illustrate ITT
# ==============================================

q1b.table <- matrix(NA, nrow=3, ncol=6)
rownames(q1b.table) <- c("treated","untreated","total")
colnames(q1b.table) <- c("mean.z1","n.z1","mean.z0","n.z0","diff","n")

q1b.table[1,1] <- round(mean(mydata$views[mydata$success==1 & mydata$hajj2006==1]), digits=3)
q1b.table[1,3] <- round(mean(mydata$views[mydata$success==0 & mydata$hajj2006==1]), digits=3)
q1b.table[2,1] <- round(mean(mydata$views[mydata$success==1 & mydata$hajj2006==0]), digits=3)
q1b.table[2,3] <- round(mean(mydata$views[mydata$success==0 & mydata$hajj2006==0]), digits=3)
q1b.table[3,1] <- round(mean(mydata$views[mydata$success==1]), digits=3)
q1b.table[3,3] <- round(mean(mydata$views[mydata$success==0]), digits=3)
q1b.table[3,5] <- round(q1b.table[3,1] - q1b.table[3,3], digits=3)
q1b.table[1,5] <- round(q1b.table[1,1] - q1b.table[1,3], digits=3)
q1b.table[2,5] <- round(q1b.table[2,1] - q1b.table[2,3], digits=3)
q1b.table[1,6] <- paste("[",round(length(mydata$views[mydata$hajj2006==1]), digits=0),"]",sep="")
q1b.table[2,6] <- paste("[",round(length(mydata$views[mydata$hajj2006==0]), digits=0),"]",sep="")
q1b.table[3,6] <- paste("[",round(length(mydata$views), digits=0),"]",sep="")
q1b.table[1,2] <- paste("[",round(length(mydata$views[mydata$success==1 & mydata$hajj2006==1]), digits=0),"]",sep="")
q1b.table[1,4] <- paste("[",round(length(mydata$views[mydata$success==0 & mydata$hajj2006==1]), digits=0),"]",sep="")
q1b.table[2,2] <- paste("[",round(length(mydata$views[mydata$success==1 & mydata$hajj2006==0]), digits=0),"]",sep="")
q1b.table[2,4] <- paste("[",round(length(mydata$views[mydata$success==0 & mydata$hajj2006==0]), digits=0),"]",sep="")
q1b.table[3,2] <- paste("[",round(length(mydata$views[mydata$success==1]), digits=0),"]",sep="")
q1b.table[3,4] <- paste("[",round(length(mydata$views[mydata$success==0]), digits=0),"]",sep="")
q1b.table[3,2] <- paste("[",round(length(mydata$views[mydata$success==1]), digits=0),"]",sep="")
q1b.table[3,4] <- paste("[",round(length(mydata$views[mydata$success==0]), digits=0),"]",sep="")

# q1b.table.out[,,x] <- q1b.table
print(q1b.table)

# ==============================================
# 1C - Estimate ITT w/o and w/ cov adjustment
# 		Form 95% confidence interval
#		Test sharp null, tau_i = 0, for all i
# ==============================================

Y <- mydata$views
Z <- mydata$success
D <- mydata$hajj2006
C <- mydata$hhid
X <- as.matrix(cbind(mydata$age, mydata$literate, mydata$female))
n.clust <- length(table(C))

# unadjusted ITT

itt <- estate(Y, Z, prob=probs, HT=FALSE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

q1c.out[,1] <- n.clust
q1c.out[,2] <- itt
q1c.out[,3] <- dispout$two.tailed.p.value.abs
q1c.out[,4] <- dispout$greater.p.value
q1c.out[,5] <- dispout$lesser.p.value

Ys <- genouts(Y, Z, ate=itt)
distout <- gendist(Ys,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)
q1c.out[,6] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
q1c.out[,7] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

# covariate adjusted ITT

itt <- estate(Y, Z, X=X, prob=probs, HT=FALSE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,X=X,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

q1c.out[,8] <- itt
q1c.out[,9] <- dispout$two.tailed.p.value.abs
q1c.out[,10] <- dispout$greater.p.value
q1c.out[,11] <- dispout$lesser.p.value

Ys <- genouts(Y, Z, ate=itt)
distout <- gendist(Ys,X=X,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)
q1c.out[,12] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
q1c.out[,13] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

q1c.out

# ==============================================
# 1D - Res-res plot
# ==============================================

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/Key/")
 pdf("q1d_unsampled.pdf")
 resresplot(Y, Z, X=X, prob=probs, scale = 1)
 dev.off()

# ==============================================
# 1E - Diff in totals (HT estimator)
# ==============================================

# unadjusted ITT

itt <- estate(Y, Z, prob=probs, HT=TRUE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,perms,prob=probs, HT=TRUE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

q1e.out[,1] <- itt
q1e.out[,2] <- dispout$two.tailed.p.value.abs
q1e.out[,3] <- dispout$greater.p.value
q1e.out[,4] <- dispout$lesser.p.value

Ys <- genouts(Y, Z, ate=itt)
distout <- gendist(Ys,perms,prob=probs, HT=TRUE)
dispout <- dispdist(distout, itt, display.plot=FALSE)
q1e.out[,5] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
q1e.out[,6] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

# covariate adjusted ITT

itt <- estate(Y, Z, X=X, prob=probs, HT=TRUE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,X=X,perms,prob=probs, HT=TRUE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

q1e.out[,7] <- itt
q1e.out[,8] <- dispout$two.tailed.p.value.abs
q1e.out[,9] <- dispout$greater.p.value
q1e.out[,10] <- dispout$lesser.p.value

Ys <- genouts(Y, Z, ate=itt)
distout <- gendist(Ys,X=X,perms,prob=probs, HT=TRUE)
dispout <- dispdist(distout, itt, display.plot=FALSE)
q1e.out[,11] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
q1e.out[,12] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

q1e.out

# ==============================================
# 1G - Shares of Compliers, Always-Takers,
#		and Never-Takers
# ==============================================

# always takers
q1g.out[,2] <- length(mydata$views[mydata$hajj==1 & mydata$success==0])/length(mydata$views[mydata$success==0])

# never takers
q1g.out[,3] <- length(mydata$views[mydata$hajj==0 & mydata$success==1])/length(mydata$views[mydata$success==1])

# compliers
q1g.out[,1] <- 1 - q1g.out[,2] - q1g.out[,3]

# check share of compliers is correct

probs <- genprobexact(Z=mydata$success, clustvar=mydata$hhid)
wt <- mydata$success / probs + (1-mydata$success) / (1-probs)
print(summary(lm(mydata$hajj2006 ~ mydata$success, weight=wt)))



# ==============================================
# 1H - CACE
# ==============================================

# unadjusted CACE

# cace
cace <- estlate(Y, D, Z, prob=probs, HT=FALSE)

# confidence intervals (from ivreg, assume constant effects and normal sampling distribution)
W <- Z/probs + (1-Z)/(1-probs)
cace.fit <- ivreg(Y ~ D, ~Z , weights=W)
print(summary(cace.fit))
ci.lower.cace <- confint(cace.fit, parm="D", level=.95)[1]
ci.upper.cace <- confint(cace.fit, parm="D", level=.95)[2]

# p values from randomization inference on the ITT
itt <- estate(Y, Z, prob=probs, HT=FALSE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

# save results
q1h.out[,1] <- cace
q1h.out[,2] <- dispout$two.tailed.p.value.abs
q1h.out[,3] <- dispout$greater.p.value
q1h.out[,4] <- dispout$lesser.p.value
q1h.out[,5] <- ci.lower.cace - (sqrt((n.clust-1)/(n.clust-2))/2)
q1h.out[,6] <- ci.upper.cace + (sqrt((n.clust-1)/(n.clust-2))/2)

# covariate adjusted CACE

# cace
cace <- estlate(Y, D, Z, X=X, prob=probs, HT=FALSE)

# confidence intervals (from ivreg, assume constant effects and normal sampling distribution)
W <- Z/probs + (1-Z)/(1-probs)
cace.fit <- ivreg(Y ~ D + X | Z + X , weights=W)
print(summary(cace.fit))
ci.lower.cace <- confint(cace.fit, parm="D", level=.95)[1]
ci.upper.cace <- confint(cace.fit, parm="D", level=.95)[2]

# p values from randomization inference on the ITT
itt <- estate(Y, Z, X=X, prob=probs, HT=FALSE)
Ys <- genouts(Y, Z, ate=0)
distout <- gendist(Ys,X=X,perms,prob=probs, HT=FALSE)
dispout <- dispdist(distout, itt, display.plot=FALSE)

# save results
q1h.out[,7] <- cace
q1h.out[,8] <- dispout$two.tailed.p.value.abs
q1h.out[,9] <- dispout$greater.p.value
q1h.out[,10] <- dispout$lesser.p.value
q1h.out[,11] <- ci.lower.cace - (sqrt((n.clust-1)/(n.clust-2))/2)
q1h.out[,12] <- ci.upper.cace + (sqrt((n.clust-1)/(n.clust-2))/2)

q1h.out

# ==============================================
# 1I - EXTREME VALUE BOUNDS
# ==============================================

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/")
q1miss <- read.dta("q1hajj.dta")

# uni <- myuni
# set.seed(uni)
# q1miss <- as.data.frame(data[sample(1:nrow(data), nrow(data), replace=T),])
# names(q1miss)

# check missing data
table(q1miss$success, useNA="always")
table(q1miss$hajj2006, useNA="always")
table(q1miss$views, useNA="always")
table(q1miss$age, useNA="always")
table(q1miss$female, useNA="always")
table(q1miss$literate, useNA="always")
table(q1miss$flag_miss_views, useNA="always")

# impute data for evb - upper bound
q1.ub.impute <- q1miss
q1.ub.impute$views <- ifelse(is.na(q1.ub.impute$views) & q1.ub.impute$success == 1, 12,  q1.ub.impute$views )
q1.ub.impute$views <- ifelse(is.na(q1.ub.impute$views) & q1.ub.impute$success == 0, -12, q1.ub.impute$views )

# impute data for evb - lower bound
q1.lb.impute <- q1miss
q1.lb.impute$views <- ifelse(is.na(q1.lb.impute$views) & q1.lb.impute$success == 1, -12, q1.lb.impute$views )
q1.lb.impute$views <- ifelse(is.na(q1.lb.impute$views) & q1.lb.impute$success == 0, 12,  q1.lb.impute$views )


# estimate upper extreme value bound for ITT

Y <- q1.ub.impute$views
Z <- q1.ub.impute$success
D <- q1.ub.impute$hajj2006
C <- q1.ub.impute$hhid
X <- as.matrix(cbind(q1.ub.impute$age, q1.ub.impute$literate, q1.ub.impute$female))

probs <- genprobexact(Z, clustvar=C)
itt.ev.upper <- estate(Y, Z, prob=probs, HT=FALSE)
itt.ev.upper.adj <- estate(Y, Z, X=X, prob=probs, HT=FALSE)

ittHT.ev.upper <- estate(Y, Z, prob=probs, HT=TRUE)
ittHT.ev.upper.adj <- estate(Y, Z, X=X, prob=probs, HT=TRUE)

# estimate lower extreme value bound for ITT

Y <- q1.lb.impute$views
Z <- q1.lb.impute$success
D <- q1.lb.impute$hajj2006
C <- q1.lb.impute$hhid
X <- as.matrix(cbind(q1.lb.impute$age, q1.lb.impute$literate, q1.lb.impute$female))

probs <- genprobexact(Z, clustvar=C)
itt.ev.lower <- estate(Y, Z, prob=probs, HT=FALSE)
itt.ev.lower.adj <- estate(Y, Z, X=X, prob=probs, HT=FALSE)

ittHT.ev.lower <- estate(Y, Z, prob=probs, HT=TRUE)
ittHT.ev.lower.adj <- estate(Y, Z, X=X, prob=probs, HT=TRUE)

# store results

q1i.out[,1] <- itt.ev.lower
q1i.out[,2] <- itt.ev.upper
q1i.out[,3] <- itt.ev.lower.adj
q1i.out[,4] <- itt.ev.upper.adj
q1i.out[,5] <- ittHT.ev.lower
q1i.out[,6] <- ittHT.ev.upper
q1i.out[,7] <- ittHT.ev.lower.adj
q1i.out[,8] <- ittHT.ev.upper.adj

q1i.out

# ==============================================
# 1J - FORMAL TEST 
# ==============================================

names(q1miss)
table(q1miss$flag_miss_views, useNA="always")

q1j.fstat <- summary(lm(flag_miss_views ~ success, data=q1miss))$fstatistic[1]

probs <- genprobexact(Z=q1miss$success, clustvar=q1miss$hhid)
perms <- genperms(Z=q1miss$success, clustvar=q1miss$hhid, maxiter=numiter)

numiter <- 10000
 f.stat.store <- rep(NA, numiter)

 for(i in 1:numiter){
	f.stat.store[i] <- summary(lm(q1miss$flag_miss_views ~ perms[,i]))$fstatistic[1]
 }

 q1j.pvalue <- mean(f.stat.store >= q1j.fstat)

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/Key/")
 pdf("q1j_unsampled.pdf")
 hist(f.stat.store, breaks=numiter/100, main=paste("Unsampled Dataset","\n Q1j: Simulated distribution of F statistic under the sharp null \n Observed F statistic: ",round(q1j.fstat, digits=3),", p-value: ",round(q1j.pvalue, digits=3),sep=""), xlab="Simulated F statistic")
abline(v=q1j.fstat, col="red")
dev.off()

q1j.out[,1] <- q1j.fstat
q1j.out[,2] <- q1j.pvalue
q1j.out




#------- PRINT STUDENT-SPECIFIC ANSWER KEY ------------

# Q1a
xtable(q1a.out, digits=3)

# Q1b 
xtable(q1b.table, digits=3)

# Q1c
xtable(q1c.out, digits=3)

# Q1d -- see pdf output

# Q1e
xtable(q1e.out, digits=3)

# Q1g
xtable(q1g.out, digits=3)

# Q1h
xtable(q1h.out, digits=3)

# Q1i
xtable(q1i.out, digits=3)



setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/Key/")
write.csv(q1a.out, "q1a_unsampled.csv")
write.csv(q1b.table, "q1b_unsampled.csv")
write.csv(q1c.out, "q1c_unsampled.csv")
write.csv(q1e.out, "q1e_unsampled.csv")
write.csv(q1g.out, "q1g_unsampled.csv")
write.csv(q1h.out, "q1h_unsampled.csv")
write.csv(q1i.out, "q1i_unsampled.csv")
write.csv(q1j.out, "q1j_unsampled.csv")
