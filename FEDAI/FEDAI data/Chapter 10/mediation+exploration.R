library(mediation)
N <- 1000

#1. Best Case Scenario
#2. Reasonable Scenario

set.seed(343)
#probs_MZ0 <- rep(.5, N)   ## Best Case Scenario
probs_MZ0 <- runif(N)      ## Reasonable Scenario
YZ0M0 <- rnorm(N, 10*probs_MZ0, 1)
YZ0M1 <- YZ0M0 + 2
YZ1M0 <- YZ0M0 + 1
YZ1M1 <- YZ0M0 + 3


M_Z0 <- rbinom(N, size=1, prob=probs_MZ0)
M_Z1 <- M_Z0
M_Z1[M_Z0==0] <- rbinom(length(M_Z1[M_Z0==0]), size=1, prob=.5)

true_ATE <- mean((YZ1M1*M_Z1 + YZ1M0 *(1-M_Z1)) - (YZ0M1*M_Z0 + YZ0M0 *(1-M_Z0)))
true_ATE

true_ACME_Z1 <- mean((YZ1M1*M_Z1 + YZ1M0 *(1-M_Z1)) - (YZ1M1*M_Z0 + YZ1M0 *(1-M_Z0)))
true_ACME_Z0 <- mean((YZ0M1*M_Z1 + YZ0M0 *(1-M_Z1)) - (YZ0M1*M_Z0 + YZ0M0 *(1-M_Z0)))

true_ACME_Z1
true_ACME_Z0

true_ADE_M1 <- mean((YZ1M1*M_Z1 + YZ1M0 *(1-M_Z1)) - (YZ0M1*M_Z1 + YZ0M0 *(1-M_Z1)))
true_ADE_M0 <- mean((YZ1M1*M_Z0 + YZ1M0 *(1-M_Z0)) - (YZ0M1*M_Z0 + YZ0M0 *(1-M_Z0)))

true_ADE_M1
true_ADE_M0

true_ATE_m <- mean(M_Z1-M_Z0)
true_ATE_m

Z <- rbinom(N, size=1, .5)

### Reveal Potential Outcomes

M.obs <- Z*M_Z1 + (1-Z)*M_Z0
Y.obs <- rep(NA, N)
Y.obs[Z==1 & M.obs==1] <- YZ1M1[Z==1 & M.obs==1]
Y.obs[Z==1 & M.obs==0] <- YZ1M0[Z==1 & M.obs==0]
Y.obs[Z==0 & M.obs==1] <- YZ0M1[Z==0 & M.obs==1]
Y.obs[Z==0 & M.obs==0] <- YZ0M0[Z==0 & M.obs==0]

## Estimate eq. 10.1 and 10.3
model.m <- lm(M.obs ~ Z)  
model.y <- lm(Y.obs ~ Z + M.obs)

# Pass model objects through mediate function
med.cont <- mediate(model.m=model.m, model.y=model.y, treat="Z", mediator="M.obs", sims=50)
summary(med.cont)

true_ACME_Z0
true_ACME_Z1
true_ADE_M0
true_ADE_M1
true_ATE


sensitivity <- medsens(x=med.cont, rho.by=.1)
plot(sensitivity)


####### Reverse Causality?

rm(list=ls())
N <- 1000
Z <- rbinom(n=N, size=1, prob=.5)
prior_belief <- rnorm(N, 7, 2)
beta_1 <- 2
beta_2 <- 1
alpha_1 <- .5
alpha_2 <- 1

Y.obs <- beta_1 * Z + beta_2*prior_belief + rnorm(N, mean=0,sd=1)
M.obs <- alpha_1*Y.obs + alpha_2*prior_belief + rnorm(N, mean=0,sd=1)


model.m <- lm(M.obs ~ Z)  
model.y <- lm(Y.obs ~ Z + M.obs)

# Pass model objects through mediate function
med.cont <- mediate(model.m=model.m, model.y=model.y, treat="Z", mediator="M.obs", sims=50)
summary(med.cont)


sensitivity <- medsens(x=med.cont, rho.by=.1)
plot(sensitivity)



















