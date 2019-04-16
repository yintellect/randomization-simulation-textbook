#library(ri)

Z <- c(1,1,1,1,0,0,0,0)
blockvar <- X <- c(0,0,1,1,0,1,0,1)
perms <- genperms(Z,blockvar=X)

rowMeans(perms)

Y1 <- c(2,3,6,1,4,8,3,5)
Y0 <- c(2,1,4,1,4,8,1,5)
alpha <- matrix(NA,ncol(perms),4)

for (i in 1:ncol(perms)) {
	Z <- perms[,i]
	Y <- Y1*Z + Y0*(1-Z)
	alpha[i,] <- lm(Y~Z*X)$coefficients
	}
	
colMeans(alpha,na.rm=TRUE)
	