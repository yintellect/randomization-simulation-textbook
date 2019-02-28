#   -----------------------------------------------------------------------
#   4.5 Analysis od Block Randomized Experiments with 
#       Treatment Probabilities that Vary by Block
#   -----------------------------------------------------------------------


library(ri)
set.seed(1)

# ----------------------------------------------------
# Calculate ATE when probabilities vary by block 
# ----------------------------------------------------

### Hypothetical potential outcome in table 4.1
Y1 <- c(5,15,12,19,17,18,24,11,16,25,18,21,17,24,27,26,30,37,43,39,36,27,33,37,48,39,42,37,53,50,51,43,55,49,48,52,59,52,55,63)
Y0 <- c(5,5,6,9,10,11,12,13,14,19,20,20,20,21,24,25,27,27,30,32,32,32,32,35,35,37,38,38,41,42,43,44,45,47,48,51,52,52,57,62)
Xweak <- c(25,12,25,27,10,24,21,25,35,28,41,38,30,20,24,26,22,34,37,21,40,34,36,37,48,46,25,21,19,44,50,48,46,47,47,39,50,46,54,42)

### block and observed outcome in table 4.2.
blockvar <- c(1,1,1,1,1,3,1,1,1,1,1,3,1,1,1,1,1,1,3,3,1,1,1,2,2,3,1,1,1,2,2,2,1,1,1,2,2,2,2,2)
Z <- c(0,1,0,1,0,0,1,1,1,1,1,0,0,1,0,1,1,0,1,0,0,1,0,1,0,1,1,1,0,0,1,0,0,1,1,1,0,1,1,0)
Y <- Y0*(1-Z) + Y1*(Z)

### generate different probabilities in blocks
prob <- genprobexact(Z,blockvar)

### calculate a weighted averaged of the ATE
cat(ate <- estate(Y,Z,Xweak,prob=prob))
# --------- the same ----------------
### Weighted least squares regression
weights <- Z/prob + (1-Z)/(1-prob)
summary(lm(Y~Z+Xweak,weights=weights))

# ----------------------------------------------------------------
# Randomization Inference for 95% Confidence Interval (Page.119)
# ----------------------------------------------------------------
### generate permutations 
perms <- genperms(Z=Z,blockvar=blockvar, maxiter = 10000)
### complete schedule under hypothesis that every school has the same ATE
Ys.est <- genouts(Y,Z,ate)
### generate sampling distribution, p-values, and confidence interval
estdist <- gendist(Ys.est,perms,X=Xweak,prob=prob,HT=FALSE)
# Displaying, summarizing and producing p-values
dispdist(estdist,ate)
# over hypothetically 10000 times replication the ATE is 
mean(estdist) 
# Our true ATE obtained from this practical experiment is
ate

# ----------------------------------------------------
# Randomization Inference under Sharp Null  (Page.119)
# ----------------------------------------------------
### complete schedule under sharp null hypothesis
Ys.est <- genouts(Y,Z,ate=0)
estdist <- gendist(Ys.est,perms,X=Xweak,prob=prob,HT=FALSE)
dispdist(estdist,ate)

# ----------------------------------------------------
# Scatter plot of residuals  (Page.119)
# ----------------------------------------------------
### Weight each observation by:
weights <- Z/prob + (1-Z)/(1-prob)
summary(lm(Y~Z+Xweak,weights=weights))
d_x<-lm(Z~Xweak,weights = weights)
plot(Xweak,Z,cex=weights)
abline(a=d_x$coefficients[1],b=d_x$coefficients[2],col="red")
plot(Xweak,d_x$residuals,cex=weights)

y_x<-lm(Y~Xweak,weights = weights)
plot(Xweak,Y,cex=weights)
abline(a=y_x$coefficients[1],b=y_x$coefficients[2],col="red")
plot(Xweak,y_x$residuals,cex=weights)
###  Res-Res plot
resresplot(Y,Z,Xweak,prob,scale=10)



### unweighted regression on dummies that indicate each block
### Assume that tretment effect are the same for all subjects (not vary from blocks)
summary(lm(Y~Z+Xweak+factor(blockvar)))

### each block's estimated ATE is wighted by (N(j)/N)*P(j)[1-P(j)]
summary(lm(Y~Z+Xweak+factor(blockvar),weights=weights))



# True Sampling Distribution

### CIs

Ys <- data.frame(Y0,Y1)
estdist <- gendist(Ys,perms,X=Xweak,prob=prob,HT=FALSE)
dispdist(estdist,ate)
mean(estdist)
mean(Y1-Y0)

