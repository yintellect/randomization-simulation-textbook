library(ri)

Z <- c(0,0,0,0,0,0,1,1,1,1,1,1)

Y0M0 = c(0,0,0,0,0,0,1,1,1,0,0,0)
Y1M0 = c(0,0,0,1,1,1,0,0,0,1,1,1)
Y0M1 = c(0,0,0,0,0,0,1,1,1,1,1,1)
Y1M1 = c(0,0,0,1,1,1,1,1,1,1,1,1)
M0 = c(0,0,1,0,0,1,0,0,1,0,0,1)
M1 = c(0,1,1,0,1,1,0,1,1,0,1,1)

perms <- genperms(Z)

coefmat <- matrix(NA,ncol(perms),3)
tcoefmat <- matrix(NA,ncol(perms),2)

for (i in 1:ncol(perms)) {
	Zri <- perms[,i]
	M <- M0*(1-Zri) + M1*Zri
	Y <- Y0M0*(1-Zri)*(1-M) + Y1M0*(Zri)*(1-M) + Y0M1*(1-Zri)*(M) + Y1M1*(Zri)*(M)
	coefmat[i,] <- lm(Y~M+Zri)$coefficients
	tcoefmat[i,] <- lm(Y~Zri)$coefficients
	}
	
colMeans(na.omit(coefmat))  # report the avg coefficients from a regression of Y on M and Z 
colMeans(na.omit(tcoefmat)) # report the avg coefficients from a regression of Y on Z 

mean(Y0M0)
mean(Y1M0)
mean(Y0M1)
mean(Y1M1)

####

Z <- c(0,0,0,0,0,0,1,1,1,1,1,1)

Y0M0 = c(0,0,0,1,0,0,1,1,1,1,1,1)
Y1M0 = c(1,0,1,1,1,1,0,0,0,1,1,1)
Y0M1 = c(0,0,0,0,1,1,1,1,1,0,0,0)
Y1M1 = c(0,0,0,1,0,0,1,1,1,1,1,1)
M0 =   c(0,0,1,0,0,1,0,0,1,0,0,1)
M1 =   c(0,1,1,0,1,1,0,1,1,0,1,1)

perms <- genperms(Z)

coefmat <- matrix(NA,ncol(perms),3)
tcoefmat <- matrix(NA,ncol(perms),2)

for (i in 1:ncol(perms)) {
	Zri <- perms[,i]
	M <- M0*(1-Zri) + M1*Zri
	Y <- Y0M0*(1-Zri)*(1-M) + Y1M0*(Zri)*(1-M) + Y0M1*(1-Zri)*(M) + Y1M1*(Zri)*(M)
	coefmat[i,] <- lm(Y~M+Zri)$coefficients
	tcoefmat[i,] <- lm(Y~Zri)$coefficients
	}

labels(lm(Y~M+Zri)$coefficients)	
colMeans(na.omit(coefmat))
colMeans(na.omit(tcoefmat))

mean(Y1M0-Y0M0)
mean(Y1M1-Y0M1)
mean(Y0M1-Y0M0)
mean(Y1M1-Y1M0)


#
#coefmeans<- rep(NA,3)
#
#for (j in 1:12) {
#	
##	Za <- Z[-j]
#
#	Y0a = Y0[-j]
#	Y1a = Y1[-j]
#	M0a = M0[-j]
#	M1a = M1[-j]
#
#	perms <- genperms(c(1,1,1,1,1,0,0,0,0,0,0))
#
#	coefmat <- matrix(NA,ncol(perms),3)
#
#	for (i in 1:ncol(perms)) {
#		Zri <- perms[,i]
#		Y <- Y0a*(1-Zri) + Y1a*Zri
#		M <- M0a*(1-Zri) + M1a*Zri
#		coefmat[i,] <- lm(Y~M+Zri)$coefficients
#		}
#	
#	coefmeans <- rbind(coefmeans,colMeans(na.omit(coefmat)))
#	
#	
#	}
#
#coefmeans <- coefmeans[-1,]
#
