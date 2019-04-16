### Midterm Code

rm(list=ls())
library(foreign)
library(ri)
library(plyr)
library(xtable)
library(sandwich)
library(lmtest)
library(stargazer)
setwd("/Users/Alex/Documents/Dropbox/Columbia/spring 2014/experiments2014/fedai homework solutions/Exams/")
nyc <- read.dta("NYC Fundraising --blocking example.dta")
nyc$block <- as.character(nyc$block)
blockmeans <- ddply(nyc, c("block"), summarize,
                    control_mean = mean(amount_donated[policy_postcard==0]),
                    control_sd = sd(amount_donated[policy_postcard==0]),
                    control_n = length(amount_donated[policy_postcard==0]),
                    treatment_mean = mean(amount_donated[policy_postcard==1]),
                    treatment_sd = sd(amount_donated[policy_postcard==1]),
                    treatment_n = length(amount_donated[policy_postcard==1]))
print(xtable(blockmeans,caption="Distribution of Donations, by Experimental Group"), include.rownames=FALSE, caption.placement="top")

fit.1 <- lm(amount_donated ~ policy_postcard, data=subset(nyc, block==1))
fit.2 <- lm(amount_donated ~ policy_postcard, data=subset(nyc, block==2))
fit.3 <- lm(amount_donated ~ policy_postcard, data=subset(nyc, block==3))
fit.4 <- lm(amount_donated ~ policy_postcard, data=subset(nyc, block==4))
fit.5 <- lm(amount_donated ~ policy_postcard, data=subset(nyc, block==5))

commarobust <- function(fit){
  coeftest(fit,vcovHC(fit,type="HC1"))
}

fit.1.r <- commarobust(fit.1)
fit.2.r <- commarobust(fit.2)
fit.3.r <- commarobust(fit.3)
fit.4.r <- commarobust(fit.4)
fit.5.r <- commarobust(fit.5)

stargazer(fit.1, fit.2, fit.3, fit.4, fit.5,type="text", omit.stat = c("f", "adj.rsq", "ser"), style="apsr",
          column.labels=c("Block 1", "Block 2", "Block 3","Block 4","Block 5"), model.numbers=FALSE,
          notes = "Robust Standard Errors in Parentheses",
          se=list(fit.1.r[,2],fit.2.r[,2],fit.3.r[,2],fit.4.r[,2],fit.5.r[,2]))



fit.all <- lm(amount_donated ~ policy_postcard + as.factor(block), data=nyc)
fit.all.r <- commarobust(fit.all)

### RI
set.seed(343)
Z <- nyc$policy_postcard
block <- nyc$block
Y <- nyc$amount_donated

probs <- genprobexact(Z=Z, blockvar=block)
ate <- estate(Y=Y, Z=Z, prob=probs)
perms <- genperms(Z=Z, blockvar=block)
Ys <- genouts(Y=Y, Z=Z, ate=0)
distout <- gendist(Ys=Ys, perms=perms,prob=probs)
pvalue.2sided <- mean(abs(distout) >= abs(ate))

fit.weighted <- lm(Y ~ Z, weights=1/probs * Z + (1/(1-probs))*(1-Z))
fit.weighted.r <- commarobust(fit.weighted)



