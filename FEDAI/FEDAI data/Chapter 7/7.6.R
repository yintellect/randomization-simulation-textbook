library(foreign)
rm(list=ls())


# Import Data
angrist <- read.dta("Angrist_et_al_AER_2006.dta")


angrist <- within(angrist,{
        read[is.na(read)] <- 0
        sex <- sex_name
        observed <- 1 - (read == 0)
        probobs <- glm(observed~(vouch0*sex)+(vouch0*phone),
                       family=binomial(link="logit"))$fitted
        weights <- 1/probobs
})
# Verify that all probabilities are less than one and greater than zero
with(angrist, {
        rbind(summary(probobs[vouch0==0]),
              summary(probobs[vouch0==1]))
})
##           Min.   1st Qu.    Median      Mean   3rd Qu.      Max.
## [1,] 0.2318797 0.2318797 0.2905849 0.2665066 0.2905849 0.3271479
## [2,] 0.2846098 0.3141727 0.3158772 0.3275862 0.3471120 0.3471120
# Coefficients for unweighted regression (restricting analysis to observed subjects)
lm(read~vouch0, data=subset(angrist, observed==1))$coefficients
## (Intercept)      vouch0
##  46.9208148   0.6827378
# Coefficients for IPW regression (restricting analysis to observed subjects)
lm(read~vouch0, weights=weights, data=subset(angrist, observed==1))$coefficients
## (Intercept)      vouch0
##  46.9654430   0.6580337