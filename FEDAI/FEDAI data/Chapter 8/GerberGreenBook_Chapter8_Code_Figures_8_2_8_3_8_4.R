rm(list = ls())

set.seed(1337)

# Establish parameters

# Number of total units in the "main experimental pop"
NS <- 30
# Number treated
ntreatedS <- 10

# True radius for spillover
radius <- .5

# Too small radius, used for misspecification
radiusW <- .25

# Too large radius, used for generating non-experimental density "effects"
radiusB <- .75

# Number of randomizations
numrands <- 10000

# Generate coordinates
ax <- rnorm(NS*2)
coordsS <- cbind(ax[1:(NS)],ax[(NS+1):(2*NS)])

# To multiply the population, ala Brewer, in completely separate groups. Setting this to 10 replicates the 100 out of 300 specification reported in the book.
numrepeater <- 1

N <- NS*numrepeater
ntreated <- ntreatedS*numrepeater
coords <- coordsS[rep(c(1:NS),numrepeater),] + cbind(2*(max(coordsS[,1])-min(coordsS[,1])+radius)*sort(rep(c(0:(numrepeater-1)),NS)),0)

# Generate non-experimental units, coordinates
NnonexpS <- 100
coordsNonS <- cbind(rnorm(NnonexpS),rnorm(NnonexpS))

Nnonexp <- NnonexpS*numrepeater

coordsNon <- coordsNonS[rep(c(1:NnonexpS),numrepeater),] + cbind(2*(max(coordsS[,1])-min(coordsS[,1])+radius)*sort(rep(c(0:(numrepeater-1)),NnonexpS)),0)


#######

# Generate distance matrix
distmat <- as.matrix(dist(coords))

# non-experimental distances to experimental units
distmatNon <- as.matrix(dist(rbind(coords,coordsNon)))[1:N,(N+1):(N+Nnonexp)]

# Calculate number of units near each unit (right, small, large)
numnear <- apply(distmat,1,function(x) sum(x < radius)) - 1
numnearW <- apply(distmat,1,function(x) sum(x < radiusW)) - 1
numnearB <- apply(distmat,1,function(x) sum(x < radiusB)) - 1

# Calculate number of units near each nonexperiment unit (right, small, large)
numnearNon <- apply(distmatNon,2,function(x) sum(x < radius))
numnearNonW <- apply(distmatNon,2,function(x) sum(x < radiusW))
numnearNonB <- apply(distmatNon,2,function(x) sum(x < radiusB))

# Base POs
Y00 <- 10 + numnearB*10

# Treatmen Effects
t01 <- -5
t10 <- 5
t11 <- -7

Y01 <- Y00 + t01
Y10 <- Y00 + t10
Y11 <- Y00 + t11

# Randomly generate treatment assignments
treat <- sample(c(rep(0,N-ntreated),rep(1,ntreated)))
#sample(sample(sample(c(rep(1,ntreated),rep(0,N-ntreated)))))

# True POs for units eligible for all treatments
mean(Y00[numnear>0])
mean(Y10[numnear>0])
mean(Y01[numnear>0])
mean(Y11[numnear>0])

# Generate non-exp POs
Y00Non <- 0 + numnearNonB
Y10Non <- Y00Non + t10

# Generate treatment conditions -- exposure condition defined by "cond"
distmattreat <- distmat + (1-treat)*100
numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treat
cond <- 10*(numtreat > 0) + treat

numtreatW <- apply(distmattreat,2,function(x) sum(x < radiusW)) - treat
condW <- 10*(numtreatW > 0) + treat

distmatNontreat <- distmatNon + (1-treat)*100
numtreatNon <- apply(distmatNontreat,2,function(x) sum(x < radius))
condNon <- 10*(numtreatNon > 0)

cond[numnear>0]
table(numnear,treat)

# Generate observed outcomes
Y <- Y00
Y[cond==1] <- Y01[cond==1]
Y[cond==11] <- Y11[cond==11]
Y[cond==10] <- Y10[cond==10]

mean(Y[treat==1]) - mean(Y[treat==0])

# Estimate probability of exposure for correct model and wrong-radius model
# Order: 00, 01, 10, 11
pmat <- pmatW <- matrix(0,N,4)

# Scalar probability (10) for non-experimental units.
pNon <- rep(0,Nnonexp)

for (i in 1:numrands) {
	treatri <- sample(treat)
	distmattreat <- distmat + (1-treatri)*100
	numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treatri
	condri <- 2*(numtreat > 0) + treatri + 1
	for (j in 1:N) pmat[j,condri[j]] <- pmat[j,condri[j]] + 1
	
	# wrong radius
	numtreat <- apply(distmattreat,2,function(x) sum(x < radiusW)) - treatri
	condri <- 2*(numtreat > 0) + treatri + 1
	for (j in 1:N) pmatW[j,condri[j]] <- pmatW[j,condri[j]] + 1
	
	# non-experimental units
	distmatNontreat <- distmatNon + (1-treatri)*100
	numtreatNon <- apply(distmatNontreat,2,function(x) sum(x < radius))
	pNon <- pNon +  (numtreatNon > 0)
	
	if (i %% 1000 == 0) cat(i,"")
	}
	
####### No one near - can estimate simple diff in means

summary(lm(Y[numnear == 0]~treat[numnear==0]))

####### Someone near - need to apply IPW

pscoremat <- pmat/numrands
pscorematW <- pmatW/numrands
pscoreNon <- pNon/numrands

#weight <- rep(NA,0)

weight <- 1/pscoremat[,1]

for (j in 1:N) {
	if (cond[j] == 01) weight[j] <- 1/pscoremat[j,2]
	if (cond[j] == 10) weight[j] <- 1/pscoremat[j,3]
	if (cond[j] == 11) weight[j] <- 1/pscoremat[j,4]
	}

######### Get Distribution

Y00mean <- Y10mean <- Y01mean <- Y11mean <- Y0meanN <- Y1meanN <- rep(NA,numrands)
Y00meanW <- Y10meanW <- Y01meanW <- Y11meanW <- Y0meanNW <- Y1meanNW <- rep(NA,numrands)
Y00meanna <- Y10meanna <- Y01meanna <- Y11meanna <- rep(NA,numrands)
Y00meanT <- Y10meanT <- Y01meanT <- Y11meanT <- Y0meanNT <- Y1meanNT <- rep(NA,numrands)
tauS <- tauNon <- rep(NA,numrands)

Nright <- sum(numnear >0)

# This is just going to iterate through the randomization distribution

for (i in 1:numrands) {
	
	# Generate treatment assignments, exposure conditions
	
	treatri <- sample(treat)
	distmattreat <- distmat + (1-treatri)*100
	numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treatri
	condri <- 10*(numtreat > 0) + treatri

	# Regenerate POs

	Yri <- Y00
	Yri[condri==1] <- Y01[condri==1]
	Yri[condri==11] <- Y11[condri==11]
	Yri[condri==10] <- Y10[condri==10]
	
	# For units with no one near

	Y1meanN[i] <- mean(Yri[numnear == 0 & treatri == 1])
	Y0meanN[i] <- mean(Yri[numnear == 0 & treatri == 0])

	# Generate weights

	weightri <- 1/pscoremat[,1]

	for (j in 1:N) {
		if (condri[j] == 01) weightri[j] <- 1/pscoremat[j,2]
		if (condri[j] == 10) weightri[j] <- 1/pscoremat[j,3]
		if (condri[j] == 11) weightri[j] <- 1/pscoremat[j,4]
	}
	
	# Estimate all effects

	Y00meanT[i] <- sum(Yri[numnear > 0 & condri == 00]*weightri[numnear > 0 & condri == 00])/Nright
	Y00mean[i] <- weighted.mean(Yri[numnear > 0 & condri == 00],weightri[numnear > 0 & condri == 00])
	Y10meanT[i] <- sum(Yri[numnear > 0 & condri == 10]*weightri[numnear > 0 & condri == 10])/Nright
	Y10mean[i] <- weighted.mean(Yri[numnear > 0 & condri == 10],weightri[numnear > 0 & condri == 10])
	Y01meanT[i] <- sum(Yri[numnear > 0 & condri == 01]*weightri[numnear > 0 & condri == 01])/Nright
	Y01mean[i] <- weighted.mean(Yri[numnear > 0 & condri == 01],weightri[numnear > 0 & condri == 01])
	Y11meanT[i] <- sum(Yri[numnear > 0 & condri == 11]*weightri[numnear > 0 & condri == 11])/Nright
	Y11mean[i] <- weighted.mean(Yri[numnear > 0 & condri == 11],weightri[numnear > 0 & condri == 11])

	Y00meanna[i] <- mean(Yri[condri==00])
	Y01meanna[i] <- mean(Yri[condri==01])
	Y10meanna[i] <- mean(Yri[condri==10])
	Y11meanna[i] <- mean(Yri[condri==11])

	tauS[i] <- mean(Yri[treatri==1]) - mean(Yri[treatri==0])

	# A naive model, where the wrong radius is specified

	numtreatW <- apply(distmattreat,2,function(x) sum(x < radiusW)) - treatri
	condriW <- 10*(numtreatW > 0) + treatri

	weightriW <- 1/pscorematW[,1]

	for (j in 1:N) {
		if (condriW[j] == 01) weightriW[j] <- 1/pscorematW[j,2]
		if (condriW[j] == 10) weightriW[j] <- 1/pscorematW[j,3]
		if (condriW[j] == 11) weightriW[j] <- 1/pscorematW[j,4]
	}


	Y1meanNW[i] <- mean(Yri[numnearW == 0 & treatri == 1])
	Y0meanNW[i] <- mean(Yri[numnearW == 0 & treatri == 0])
	Y00meanW[i] <- weighted.mean(Yri[numnearW > 0 & condriW == 00],weightriW[numnearW > 0 & condriW == 00])
	Y10meanW[i] <- weighted.mean(Yri[numnearW > 0 & condriW == 10],weightriW[numnearW > 0 & condriW == 10])
	Y01meanW[i] <- weighted.mean(Yri[numnearW > 0 & condriW == 01],weightriW[numnearW > 0 & condriW == 01])
	Y11meanW[i] <- weighted.mean(Yri[numnearW > 0 & condriW == 11],weightriW[numnearW > 0 & condriW == 11])

	# Using non-experimental units

	distmatNontreat <- distmatNon + (1-treatri)*100
	numtreatNon <- apply(distmatNontreat,2,function(x) sum(x < radius))
	condriNon <- 10*(numtreatNon > 0)

	tauNon[i] <- weighted.mean(Y10Non[numnearNon > 0 & condriNon == 10],1/pscoreNon[numnearNon > 0 & condriNon == 10]) - weighted.mean(Y00Non[numnearNon > 0 & condriNon == 00],1/(1-pscoreNon)[numnearNon > 0 & condriNon == 00])

	if (i %% 1000 == 0) cat(i,"")
	}

######### Details the true means for the exposure-eligible units

mean(Y00[numnear>0])
summary(Y00mean)
summary(Y00meanT)

mean(Y10[numnear>0])
summary(Y10mean)
summary(Y10meanT)

mean(Y01[numnear>0])
summary(Y01mean)
summary(Y01meanT)

mean(Y11[numnear>0])
summary(Y11mean)
summary(Y11meanT)

### This is the point estimates for the naive diffs in means

mean(Y[cond==01]) - mean(Y[cond==0])
mean(Y[cond==10]) - mean(Y[cond==0])
mean(Y[cond==11]) - mean(Y[cond==0])

### Now the correct, IPW diff-in-means

# stratum 1 - no spillover-eligible

weighted.mean(Y[cond==01 & numnear == 0],weights=weight[cond==01 & numnear == 0]) -  weighted.mean(Y[cond==0 & numnear == 0],weights=weight[cond==0 & numnear == 0])

# stratum 2 - spillover-eligible

weighted.mean(Y[cond==01 & numnear > 0],weights=weight[cond==01 & numnear > 0]) - weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])
weighted.mean(Y[cond==10 & numnear > 0],weights=weight[cond==10 & numnear > 0])- weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])
weighted.mean(Y[cond==11 & numnear > 0],weights=weight[cond==11 & numnear > 0]) - weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])

# non-experimental

weighted.mean(Y10Non[numnearNon > 0 & condNon == 10],1/pscoreNon[numnearNon > 0 & condNon == 10]) - weighted.mean(Y00Non[numnearNon > 0 & condNon == 00],1/(1-pscoreNon)[numnearNon > 0 & condNon == 00])

### True effects

mean(Y01[numnear>0]-Y00[numnear>0])
mean(Y10[numnear>0]-Y00[numnear>0])
mean(Y11[numnear>0]-Y00[numnear>0])

mean(Y10Non[numnearNon>0]-Y00Non[numnearNon>0])


### Describe the randomization distributions of all effects

# Correct, spillover-ineligible
summary(Y1meanN-Y0meanN)
sd(Y1meanN-Y0meanN,na.rm=TRUE)

# Correct, spillover-eligible
summary(Y01mean-Y00mean)
sd(Y01mean-Y00mean,na.rm=TRUE)
summary(Y10mean-Y00mean)
sd(Y10mean-Y00mean,na.rm=TRUE)
summary(Y11mean-Y00mean)
sd(Y11mean-Y00mean,na.rm=TRUE)

### Assuming no spillover
summary(tauS)

### No weights
summary(Y01meanna-Y00meanna)
summary(Y10meanna-Y00meanna)
summary(Y11meanna-Y00meanna)

### Wrong model of spillover
summary(Y1meanNW-Y0meanNW)
summary(Y01meanW-Y00meanW)
summary(Y10meanW-Y00meanW)
summary(Y11meanW-Y00meanW)

### Non-experimental units
summary(tauNon)

######## Put together some plots

plot(coords,type="n",asp=1,axes=FALSE,xlab="",ylab="")
symbols(coords[treat==1,],circles=rep(radius,ntreated),inches=FALSE,fg="grey50",bg="#00000011",add=TRUE)
points(coords[treat==0,])
points(coords[treat==1,],pch=19)

########

#####

# Now, we're going to repeat everything above with hypothesized potential outcomes, to estimate confidence intervals. No difference in interpreting the code, everything is just estimated now.

tauestN <- weighted.mean(Y[cond==01 & numnear == 0],weights=weight[cond==01 & numnear == 0]) - weighted.mean(Y[cond==0 & numnear == 0],weights=weight[cond==0 & numnear == 0])

tau01est <- weighted.mean(Y[cond==01 & numnear > 0],weights=weight[cond==01 & numnear > 0]) - weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])
tau10est <- weighted.mean(Y[cond==10 & numnear > 0],weights=weight[cond==10 & numnear > 0])- weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])
tau11est <- weighted.mean(Y[cond==11 & numnear > 0],weights=weight[cond==11 & numnear > 0])- weighted.mean(Y[cond==0 & numnear > 0],weights=weight[cond==0 & numnear > 0])

tauestNon <- weighted.mean(Y10Non[numnearNon > 0 & condNon == 10],1/pscoreNon[numnearNon > 0 & condNon == 10]) - weighted.mean(Y00Non[numnearNon > 0 & condNon == 00],1/pscoreNon[numnearNon > 0 & condNon == 00])

Y00est <- Y01est <- Y10est <- Y11est <- Y

Y00est[cond == 01] <- Y[cond == 01] - tau01est
Y00est[cond == 10] <- Y[cond == 10] - tau10est
Y00est[cond == 11] <- Y[cond == 11] - tau11est

Y01est[cond == 00] <- Y[cond == 00] + tau01est
Y01est[cond == 10] <- Y[cond == 10] - tau10est + tau01est
Y01est[cond == 11] <- Y[cond == 11] - tau11est + tau01est

Y10est[cond == 00] <- Y[cond == 00] + tau10est
Y10est[cond == 01] <- Y[cond == 01] - tau01est + tau10est
Y10est[cond == 11] <- Y[cond == 11] - tau11est + tau10est

Y11est[cond == 00] <- Y[cond == 00] + tau11est
Y11est[cond == 01] <- Y[cond == 01] - tau01est + tau11est
Y11est[cond == 10] <- Y[cond == 10] - tau10est + tau11est

Y0est <- Y1est <- Y
Y0est[treat == 1] <- Y[treat == 1] - tauestN
Y1est[treat == 0] <- Y[treat == 0] + tauestN

######

YNon <- Y00Non
YNon[condNon==10] <- Y10Non[condNon==10] 

Y00estNon <- Y10estNon <- YNon
Y00estNon[condNon == 10] <- YNon[condNon == 10] - tauestNon
Y10estNon[condNon == 00] <- YNon[condNon == 00] + tauestNon


Y00meanRos <- Y10meanRos <- Y01meanRos <- Y11meanRos <- Y0meanRosN <- Y1meanRosN <- rep(NA,numrands)
Y00meanRosna <- Y10meanRosna <- Y01meanRosna <- Y11meanRosna <- tauSRos <- tauNonRos <- rep(NA,numrands)

Nright <- sum(numnear >0)

for (i in 1:numrands) {
	treatri <- sample(treat)
	distmattreat <- distmat + (1-treatri)*100
	numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treatri
	condri <- 10*(numtreat > 0) + treatri

	Yri <- Y00est
	Yri[condri==1] <- Y01est[condri==1]
	Yri[condri==11] <- Y11est[condri==11]
	Yri[condri==10] <- Y10est[condri==10]

	Y1meanRosN[i] <- mean(Y1est[numnear == 0 & treatri == 1])
	Y0meanRosN[i] <- mean(Y0est[numnear == 0 & treatri == 0])

	weightri <- 1/pscoremat[,1]

	for (j in 1:N) {
		if (condri[j] == 01) weightri[j] <- 1/pscoremat[j,2]
		if (condri[j] == 10) weightri[j] <- 1/pscoremat[j,3]
		if (condri[j] == 11) weightri[j] <- 1/pscoremat[j,4]
	}
	

	Y00meanRos[i] <- weighted.mean(Yri[numnear > 0 & condri == 00],weightri[numnear > 0 & condri == 00])
	Y10meanRos[i] <- weighted.mean(Yri[numnear > 0 & condri == 10],weightri[numnear > 0 & condri == 10])
	Y01meanRos[i] <- weighted.mean(Yri[numnear > 0 & condri == 01],weightri[numnear > 0 & condri == 01])
	Y11meanRos[i] <- weighted.mean(Yri[numnear > 0 & condri == 11],weightri[numnear > 0 & condri == 11])

#	Y00meanRosna[i] <- mean(Yri[condri==00])
#	Y01meanRosna[i] <- mean(Yri[condri==01])
#	Y10meanRosna[i] <- mean(Yri[condri==10])
#	Y11meanRosna[i] <- mean(Yri[condri==11])

	tauSRos[i] <- mean(Yri[treatri==1]) - mean(Yri[treatri==0])
	
	distmatNontreat <- distmatNon + (1-treatri)*100
	numtreatNon <- apply(distmatNontreat,2,function(x) sum(x < radius))
	condriNon <- 10*(numtreatNon > 0)
	
	tauNonRos[i] <- weighted.mean(Y10estNon[numnearNon > 0 & condriNon == 10],1/pscoreNon[numnearNon > 0 & condriNon == 10]) - weighted.mean(Y00estNon[numnearNon > 0 & condriNon == 00],1/(1-pscoreNon)[numnearNon > 0 & condriNon == 00])

	if (i %% 1000 == 0) cat(i,"")
	}

# Confidence intervals for correct estimators

quantile(Y1meanRosN - Y0meanRosN,na.rm=TRUE,c(0.025,.5,0.975))

quantile(Y01meanRos - Y00meanRos,na.rm=TRUE,c(0.025,.5,0.975))
quantile(Y10meanRos - Y00meanRos,na.rm=TRUE,c(0.025,.5,0.975))
quantile(Y11meanRos - Y00meanRos,na.rm=TRUE,c(0.025,.5,0.975))

quantile(tauNonRos,na.rm=TRUE,c(0.025,.5,0.975))

# More display plots, saving data

#save.image("10of30.rData")

cbind(numnear,numnearB,Y00,Y01,Y10,Y11)

distmattreat <- distmat + (1-treat)*100
numtreat <- apply(distmattreat,2,function(x) sum(x < radius)) - treat

cbind(treat,cond,Y)

## Plot Figure 8.2
plot(coords,type="n",asp=1,axes=FALSE,xlab="",ylab="")
symbols(coords,circles=rep(radius,N),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords)

## Plot Figure 8.3
plot(coords,type="n",asp=1,axes=FALSE,xlab="",ylab="")
symbols(coords[treat==1,],circles=rep(radius,ntreated),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords[cond==00,])
points(coords[cond==10,],pch=19,col="grey")
points(coords[cond==10,])
points(coords[treat==1,],pch=19)

## Plot Figure 8.4
plot(coords,type="n",asp=1,axes=FALSE,xlab="",ylab="")
symbols(coords[treat==1,],circles=rep(radius,ntreated),inches=FALSE,fg="grey50",bg="#00000000",add=TRUE)
points(coords[cond==00,])
points(coords[cond==10,],pch=19,col="grey")
points(coords[cond==10,])
points(coords[treat==1,],pch=19)
points(coordsNon,pch=4)

# Output Nonexp CSV

YNon <- Y00Non
YNon[condNon==10] <- Y10Non[condNon==10]

nonexpCSV <- data.frame(numnearNonB,1-pscoreNon,pscoreNon,Y00Non,Y10Non,condNon,YNon)

colnames(nonexpCSV) <- list("No. Hotspots","Pr(00)","Pr(10)","Y00","Y10","Exposure","Y")

nonexpCSV
