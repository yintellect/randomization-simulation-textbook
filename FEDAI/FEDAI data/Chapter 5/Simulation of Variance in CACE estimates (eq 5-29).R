# simulation to illustrate the adequacy of the approximate standard errors described in equation (5.29)

library(AER)
library(sandwich)

numiter <- 1000

estT <- ITT <- ITTD <- tau <- estV <- rep(NA,numiter)

for (iter in 1:numiter) {

N <- 2500

Z <- rbinom(N,1,.5)    # this is simple randomization; could do complete random assignment instead
C <- rbinom(N,1,.4)    # generate the compliance rate
D <- Z*C

Y0 <- rnorm(N) + C*5
Y1 <- Y0+0+2*rnorm(N)  # change additive and random effects as desired
                       # the approximate variance should work well when the CACE is close to zero and the
                       # sample is large enough to recover a zero

Y <- D*Y1 + (1-D)*Y0

tau[iter] <- mean((Y1-Y0)[C==1])
estT[iter] <- cov(Y,Z)/cov(D,Z)
estV[iter] <- vcovHC(ivreg(Y~D,~Z))[2,2]
ITT[iter] <- mean(Y[Z==1]) - mean(Y[Z==0])
ITTD[iter] <- mean(D[Z==1]) - mean(D[Z==0])
cat(iter,"")
}

var(estT) # true variance
mean(estV) # huber-white variance
var(ITT)/mean(ITTD)^2 # "approximate variance"
(var(ITT)+mean(estT)^2*var(ITTD)-2*mean(estT)*cov(ITT,ITTD))/mean(ITTD)^2 # variance of linearized statistic

