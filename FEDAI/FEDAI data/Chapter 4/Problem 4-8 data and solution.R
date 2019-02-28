library(ri)

set.seed(1234567)

# Y	D	Duplicate	N
# 0	0	No	250
# 1	0	No	50
# 0	1	No	250
# 1	1	No	50
# 0	0	Yes	25
# 1	0	Yes	25
# 0	1	Yes	65
# 1	1	Yes	85

dupdata <- data.frame(
Y = rep(c(0,1,0,1,0,1,0,1),times=c(250,50,250,50,25,25,65,85)),
D = rep(c(0,0,1,1,0,0,1,1),times=c(250,50,250,50,25,25,65,85)),
dup = rep(c("No","No","No","No","Yes","Yes","Yes","Yes"),times=c(250,50,250,50,25,25,65,85)))

Y <- dupdata$Y
Z <- dupdata$D


duplicate_strata <- as.integer(dupdata$dup)-1
block <- duplicate_strata


perms <- genperms(Z,blockvar=block,maxiter=10000)

probs <- genprobexact(Z,blockvar=block)

ate <- estate(Y,Z,prob=probs)

Ys <- genouts(Y,Z,ate=0)

distout <- gendist(Ys,perms,prob=probs)

ate

dispdist(distout,ate)

