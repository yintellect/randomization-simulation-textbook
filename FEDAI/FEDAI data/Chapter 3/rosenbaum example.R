library(ri)

# Small bugfix for invert.ci code -- will make it into next version of the package

invert.ci <- function(Y,Z,prob,perms,targetp) {

	ate <- estate(Y,Z,prob=prob)
	Ys <- genouts(Y,Z,ate)
	distro <- gendist(Ys,perms,prob=prob)
	
	mindistro <- quantile(distro,mean(c(targetp,0)))
	maxdistro <- quantile(distro,mean(c(targetp,1)))
		
	ATEg <- ATEgorig <- quantile(distro,targetp)
	bw <- min(abs(mindistro-ATEg),abs(maxdistro-ATEg))
	
	Ys1 <- genouts(Y-ATEg*Z,Z,0)
	testS <- estate(Y-ATEg*Z,Z,prob=prob)
	dist1 <- gendist(Ys1,perms,prob=prob)
	pguess <- mean(dist1 >= testS)

if (pguess >= targetp) bound <- ATEg - bw
if (pguess < targetp) bound <- ATEg + bw


# see if bound is good enough; might need to go farther

	YsM <- genouts(Y-bound*Z,Z,0)
	testM <- estate(Y-bound*Z,Z,prob=prob)
	distM <- gendist(YsM,perms,prob=prob)
	pguessM <- mean(distM >= testM)

counter.max <- 100
counter <- 0

while (pguess > targetp & pguessM > targetp) {
	temp <- ATEg
	ATEg <- bound
	bound <- ATEg - bw

	YsM <- genouts(Y-bound*Z,Z,0)
	testM <- estate(Y-bound*Z,Z,prob=prob)
	distM <- gendist(YsM,perms,prob=prob)
	pguessM <- mean(distM >= testM)	
	counter <- counter + 1
	if (counter >= counter.max) stop("Cannot Reach p.")
	}
	

while (pguess < targetp & pguessM < targetp) {
	temp <- ATEg
	ATEg <- bound
	bound <- ATEg + bw

	YsM <- genouts(Y-bound*Z,Z,0)
	testM <- estate(Y-bound*Z,Z,prob=prob)
	distM <- gendist(YsM,perms,prob=prob)
	pguessM <- mean(distM >= testM)
	counter <- counter + 1
	if (counter >= counter.max) stop("Cannot Reach p.")
}


findroot <- function(ATEg,targetp) {
	Ys1 <- genouts(Y-ATEg*Z,Z,0)
	testS <- estate(Y-ATEg*Z,Z,prob=prob)
	dist1 <- gendist(Ys1,perms,prob=prob)
	return(mean(dist1 >= testS) - targetp)
	}
	
if (pguessM == targetp) {
	ATEg <- bound
	pguess <- targetp
	}
	
if (pguess != targetp) {
	lowint <- uniroot(findroot,c(bound,ATEg),targetp=targetp)
	lowintM <- lowint$root
} else lowintM <- ATEg

return(lowintM)
}

##### Code begins -- parameters

ntreat <- 500
ncontrol <- 3500
N <- ntreat+ncontrol

# ratio of treatment to control standard deviations
ratio <- 2

Y0 <- rnorm(N)
Y0 <- Y0 - mean(Y0)
Y1 <- rnorm(N)*ratio
Y1 <- Y1 - mean(Y1)

# True ATE is Zero.

Z <- c(rep(1,ntreat),rep(0,ncontrol))

perms <- genperms(Z,maxiter=500)

upperci <- lowerci <- rep(NA,ncol(perms))
Nupperci <- Nlowerci <- rep(NA,ncol(perms))
Pupperci <- Plowerci <- rep(NA,ncol(perms))

for (i in 1:ncol(perms)) {
	Zri <- perms[,i]
	Y <- Y0*(1-Zri) + Y1*Zri
	upperci[i] <- invert.ci(Y,Zri,prob=rep(mean(Zri),N),perms=perms,targetp=0.975)
	lowerci[i] <- invert.ci(Y,Zri,prob=rep(mean(Zri),N),perms=perms,targetp=0.025)
	seest <- (var(Y[Zri==1])/sum(Zri) + var(Y[Zri==0])/sum(1-Zri))^.5
	ateest <- mean(Y[Zri==1]) - mean(Y[Zri==0])
	Nupperci[i] <- ateest + 1.96*seest
	Nlowerci[i] <- ateest - 1.96*seest
	Ys <- genouts(Y,Zri,ate=ateest)
	cis <- quantile(gendist(Ys,perms,prob=rep(mean(Zri),N)),c(0.025,0.975))
	Plowerci[i] <- cis[1]
	Pupperci[i] <- cis[2]
	cat(i,"")
}

# Coverage of Neyman + normal approx
mean(Nupperci >= 0 & Nlowerci <= 0,na.rm=TRUE)
# Coverage of Rosenbaum method
mean(upperci >= 0 & lowerci <= 0,na.rm=TRUE)
# Coverage of Aronow method
mean(Pupperci >= 0 & Plowerci <= 0,na.rm=TRUE)

# Summarizing intervals
summary(Nupperci)
summary(upperci)
summary(Pupperci)

summary(Nlowerci)
summary(lowerci)
summary(Plowerci)
