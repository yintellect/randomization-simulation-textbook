# ==============================================
# W4368 EXPERIMENTAL RESEARCH -- SPRING 2013
# MIDTERM EXAM KEY
# TA: Al Fang
# Date: 20 Apr 2013
# ==============================================

rm(list=ls(all=T))

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/")

library(ri)
library(foreign)
library(xtable)
library(AER)
library(plyr)

# Read in UNIs and student names to set seeds

stinfo <- read.csv("seeds.csv", header=T)
names(stinfo) <- c("name","uni","seed")

# Create objects to hold results

q2a.out <- matrix(NA, nrow=0, ncol=4)
q2b.out <- matrix(NA, nrow=0, ncol=9) # within block ITT
q2e.out <- matrix(NA, nrow=0, ncol=9) # within block ITT_d

# block weighted ITT (diff in means)
q2c.out <- matrix(NA, nrow=nrow(stinfo), ncol=13)
colnames(q2c.out) <- c("k","itt","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","itt.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q2c.out) <- stinfo$name

# block weighted ITT (HT- diff in totals)
q2d.out <- matrix(NA, nrow=nrow(stinfo), ncol=13)
colnames(q2d.out) <- c("k","itt","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","itt.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q2d.out) <- stinfo$name

# block weighted CACE
q2f.out <- matrix(NA, nrow=nrow(stinfo), ncol=13)
colnames(q2f.out) <- c("k","cace","pval.twotailed","pval.greater","pval.lesser","ci.lb","ci.ub","cace.adj","pval.twotailed.adj","pval.greater.adj","pval.lesser.adj","ci.lb.adj","ci.ub.adj")
rownames(q2f.out) <- stinfo$name

# randomization check
q2g.out <- matrix(NA, nrow=nrow(stinfo), ncol=2)
colnames(q2g.out) <- c("f.stat","p.value")
rownames(q2g.out) <- stinfo$name

# Loop over each student's UNI number

for(x in 1:nrow(stinfo)){


myuni <- stinfo$seed[x]

# ============================================================================================
# ============================================================================================
# Problem 2
# ============================================================================================
# ============================================================================================

# Read in raw data, prepare person-specific dataset

setwd("/Users/al/Dropbox/FEDAI Homework Solutions/Midterm/")
data <- read.dta("q2acorn.dta", convert.factors=F)

uni <- myuni
set.seed(uni)
q2data.orig <- as.data.frame(data[sample(1:nrow(data), nrow(data), replace=T),])

# check data, create subject identifier
head(q2data.orig)
q2data.orig$subjectid <- 1:nrow(q2data.orig)

# create correct "persons" variable
newpersons <- as.data.frame(table(q2data.orig$hhid))
names(newpersons) <- c("hhid","persons")

# merge hhid level file back to subject level file.
q2data <- merge(q2data.orig[,!(names(q2data.orig) %in% "persons")], newpersons, by="hhid", all.x=T, all.y=T)
q2data <- q2data[order(q2data$subjectid),]
head(q2data)
names(q2data)
table(q2data$hhid, q2data$persons)

# initialize variables
Y <- q2data$vote03
D <- q2data$contact
Z <- q2data$treat2
B <- q2data$persons
C <- q2data$hhid
X <- as.matrix(cbind(q2data$vote02, q2data$vote00, q2data$precinct, q2data$age))

table(Y)
table(D)
table(Z)
table(Z, D, dnn=c("Z","D"))
table(B)
table(C)
table(C, B, dnn=c("C","B"))

# =================================================
# 2G - Randomization check
# =================================================

numiter <- 10000
probs <- genprobexact(Z, blockvar=B, clustvar=C)
perms <- genperms(Z,blockvar=B,clustvar=C,maxiter=10000)

q2g.fstat <- summary(lm(Z ~ X))$fstatistic[1]

f.stat.store <- rep(NA, numiter)

for(i in 1:numiter){
	f.stat.store[i] <- summary(lm(perms[,i] ~ X))$fstatistic[1]
 }

 q2g.pvalue <- mean(f.stat.store >= q2g.fstat)

 setwd("/Users/al/Dropbox/Teaching/S13 Experiments/Midterm/Key/")
 pdf(paste("q2g_",stinfo$uni[x],".pdf",sep=""))
 hist(f.stat.store, breaks=numiter/100, main=paste("Student: ",stinfo$name[x],", UNI: ",stinfo$uni[x],"\n Q2g: Simulated distribution of F statistic under the sharp null \n Observed F statistic: ",round(q2g.fstat, digits=3),", p-value: ",round(q2g.pvalue, digits=3),sep=""), xlab="Simulated F statistic")
 abline(v=q2g.fstat, col="red")
 dev.off()

 q2g.out[x,1] <- q2g.fstat
 q2g.out[x,2] <- q2g.pvalue



# ==============================================
# 2A - Probability of assignment for each block
# 2B - Estimate ITT for each block
# ==============================================


q2a.temp <- matrix(NA, nrow=length(names(table(B))), ncol=4)
q2b.temp <- matrix(NA, nrow=length(names(table(B))), ncol=9)
q2e.temp <- matrix(NA, nrow=length(names(table(B))), ncol=9)

q2a.temp[,3] <- as.numeric(names(table(B)))	# block b
q2b.temp[,3] <- as.numeric(names(table(B)))	# block b
q2e.temp[,3] <- as.numeric(names(table(B)))	# block b

for(b in 1:length(names(table(B)))){
q2a.temp[b,1] <- as.character(stinfo$name[x])
q2a.temp[b,2] <- as.character(stinfo$uni[x])
q2a.temp[b,4] <- round(as.numeric(names(table(probs[B== q2a.temp[b,3]  ]))), digits=3)	# pr(assign to treat) in block b	

# within block ITT
q2b.temp[b,4] <- round(mean(Y[Z==1 & B==q2a.temp[b,3]]), digits=3)
q2b.temp[b,5] <- length(Y[Z==1 & B==q2a.temp[b,3]])
q2b.temp[b,6] <- round(mean(Y[Z==0 & B==q2a.temp[b,3]]), digits=3)
q2b.temp[b,7] <- length(Y[Z==0 & B==q2a.temp[b,3]])
q2b.temp[b,8] <- round(mean(Y[Z==1 & B==q2a.temp[b,3]]) - mean(Y[Z==0 & B==q2a.temp[b,3]]), digits=3)
q2b.temp[b,9] <- length(Y[B==q2a.temp[b,3]])
q2b.temp[b,1] <- as.character(stinfo$name[x])
q2b.temp[b,2] <- as.character(stinfo$uni[x])

# within block ITT_D
q2e.temp[b,4] <- round(mean(D[Z==1 & B==q2a.temp[b,3]]), digits=3)
q2e.temp[b,5] <- length(D[Z==1 & B==q2a.temp[b,3]])
q2e.temp[b,6] <- round(mean(D[Z==0 & B==q2a.temp[b,3]]), digits=3)
q2e.temp[b,7] <- length(D[Z==0 & B==q2a.temp[b,3]])
q2e.temp[b,8] <- round(mean(D[Z==1 & B==q2a.temp[b,3]]) - mean(D[Z==0 & B==q2a.temp[b,3]]), digits=3)
q2e.temp[b,9] <- length(D[B==q2a.temp[b,3]])
q2e.temp[b,1] <- as.character(stinfo$name[x])
q2e.temp[b,2] <- as.character(stinfo$uni[x])

}

q2a.out <- rbind(q2a.out, q2a.temp)
q2b.out <- rbind(q2b.out, q2b.temp)
q2e.out <- rbind(q2e.out, q2e.temp)




# =================================================
# 2C - Estimate block weighted ITT, sharp null, CI
# 2D - ITT using Diff in Totals
# 2F - CACE
# =================================================

# Sanitize blocks - drop if there is no overlap

		blockNs <- as.vector(table(B))					# number of cases within each block
		num.blocks <- length(blockNs)					# number of blocks
		totalN <- sum(blockNs)							# total N
		blockvals <- names(table(B))					# names of blocks
		
		bnums.overlap <- NULL
		for(m in 1:(num.blocks)){	
			assign.b <- Z[B==blockvals[m]]	# treatment assignments in each block
			if (length(table(assign.b))==2) bnums.overlap <- rbind(bnums.overlap, blockvals[m])
		}	
		
		dat <- as.data.frame(q2data[q2data$persons %in% as.vector(bnums.overlap),] )
		dim(dat)
		names(dat)

	# initialize variables needed for analysis
		Y <- dat$vote03
		D <- dat$contact
		Z <- dat$treat2
		B <- dat$persons
		C <- dat$hhid
		X <- as.matrix(cbind(dat$vote02, dat$vote00, dat$precinct, dat$age))
		
	# probabilities of assignment to treatment and
	# all possible random assignments

		probs <- genprobexact(Z, blockvar=B, clustvar=C)
		perms <- genperms(Z,blockvar=B,clustvar=C,maxiter=10000)
		n.clust <- length(table(C))

	# 2C calculate block-weighted ITT, ri pvals, 95% ci

		# without covariate adjustment

		itt <- estate(Y,Z,prob=probs,HT=FALSE)	
		Ys <- genouts(Y, Z, ate=0)
		distout <- gendist(Ys, perms, prob=probs, HT=FALSE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)		

		q2c.out[x,1] <- n.clust
		q2c.out[x,2] <- itt
		q2c.out[x,3] <- dispout$two.tailed.p.value.abs
		q2c.out[x,4] <- dispout$greater.p.value
		q2c.out[x,5] <- dispout$lesser.p.value

		Ys <- genouts(Y, Z, ate=itt)
		distout <- gendist(Ys,perms,prob=probs, HT=FALSE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)
		q2c.out[x,6] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2c.out[x,7] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

		# covariate adjusted ITT

		itt <- estate(Y, Z, X=X, prob=probs, HT=FALSE)
		Ys <- genouts(Y, Z, ate=0)
		distout <- gendist(Ys,X=X,perms,prob=probs, HT=FALSE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)

		q2c.out[x,8] <- itt
		q2c.out[x,9] <- dispout$two.tailed.p.value.abs
		q2c.out[x,10] <- dispout$greater.p.value
		q2c.out[x,11] <- dispout$lesser.p.value

		Ys <- genouts(Y, Z, ate=itt)
		distout <- gendist(Ys,X=X,perms,prob=probs, HT=FALSE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)
		q2c.out[x,12] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2c.out[x,13] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)


	# 2D calculate block-weighted ITT (HT diff in totals), ri pvals, 95% ci		

		# without covariate adjustment

		itt <- estate(Y,Z,prob=probs,HT=TRUE)	
		Ys <- genouts(Y, Z, ate=0)
		distout <- gendist(Ys, perms, prob=probs, HT=TRUE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)		

		q2d.out[x,1] <- n.clust
		q2d.out[x,2] <- itt
		q2d.out[x,3] <- dispout$two.tailed.p.value.abs
		q2d.out[x,4] <- dispout$greater.p.value
		q2d.out[x,5] <- dispout$lesser.p.value

		Ys <- genouts(Y, Z, ate=itt)
		distout <- gendist(Ys,perms,prob=probs, HT=TRUE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)
		q2d.out[x,6] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2d.out[x,7] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

		# covariate adjusted ITT

		itt <- estate(Y, Z, X=X, prob=probs, HT=TRUE)
		Ys <- genouts(Y, Z, ate=0)
		distout <- gendist(Ys,X=X,perms,prob=probs, HT=TRUE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)

		q2d.out[x,8] <- itt
		q2d.out[x,9] <- dispout$two.tailed.p.value.abs
		q2d.out[x,10] <- dispout$greater.p.value
		q2d.out[x,11] <- dispout$lesser.p.value

		Ys <- genouts(Y, Z, ate=itt)
		distout <- gendist(Ys,X=X,perms,prob=probs, HT=TRUE)
		dispout <- dispdist(distout, itt, display.plot=FALSE)
		q2d.out[x,12] <- dispout$quantile[1] - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2d.out[x,13] <- dispout$quantile[2] + (sqrt((n.clust-1)/(n.clust-2))/2)

	# 2F calculate block-weighted CACE, 95% ci

		# unadjusted CACE
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
		q2f.out[x,1] <- cace
		q2f.out[x,2] <- dispout$two.tailed.p.value.abs
		q2f.out[x,3] <- dispout$greater.p.value
		q2f.out[x,4] <- dispout$lesser.p.value
		q2f.out[x,5] <- ci.lower.cace - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2f.out[x,6] <- ci.upper.cace + (sqrt((n.clust-1)/(n.clust-2))/2)

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
		q2f.out[x,7] <- cace
		q2f.out[x,8] <- dispout$two.tailed.p.value.abs
		q2f.out[x,9] <- dispout$greater.p.value
		q2f.out[x,10] <- dispout$lesser.p.value
		q2f.out[x,11] <- ci.lower.cace - (sqrt((n.clust-1)/(n.clust-2))/2)
		q2f.out[x,12] <- ci.upper.cace + (sqrt((n.clust-1)/(n.clust-2))/2)



} #END LOOP OVER STUDENTS



#------- PRINT STUDENT-SPECIFIC ANSWER KEY ------------

# Q2a
colnames(q2a.out) <- c("Student","UNI","Block (\# Persons)","Pr(Assign to Treatment)")
q2a.out

for(y in 1:nrow(stinfo)){
	
	print(as.character(stinfo$name[y]))
	print(xtable(q2a.out[q2a.out[,1]==as.character(stinfo$name[y]),3:4]))
}

# Q2b 
for(y in 1:nrow(stinfo)){

	print(as.character(stinfo$name[y]))
	print(xtable(q2b.out[q2b.out[,1]==as.character(stinfo$name[y]),3:9]))

}

# Q2c

xtable(q2c.out)

# Q2d

xtable(q2d.out)

# Q2e
for(y in 1:nrow(stinfo)){

	print(as.character(stinfo$name[y]))
	print(xtable(q2e.out[q2e.out[,1]==as.character(stinfo$name[y]),3:9]))

}

# Q2f

xtable(q2f.out)

setwd("/Users/al/Dropbox/Teaching/S13 Experiments/Midterm/Key/")
write.csv(q2a.out, "q2a.csv")
write.csv(q2b.out, "q2b.csv")
write.csv(q2c.out, "q2c.csv")
write.csv(q2d.out, "q2d.csv")
write.csv(q2e.out, "q2e.csv")
write.csv(q2f.out, "q2f.csv")
write.csv(q2g.out, "q2g.csv")