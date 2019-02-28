# program to generate Box 7.1
rm(list=ls())
library(foreign)

# Load raw data
dataR <- read.dta("Chapter 7_Angrist, Bettinger, and Kremer (2006) Dataset.dta")

# Subset data, keeping if age >= 9 & age <= 25 & checkid == 1
dataS <- dataR[dataR$age >= 9 & dataR$age <= 25 & dataR$checkid == 1,]

# Fix NA
dataS$read[is.na(dataS$read)] <- 0

# having prepped the data for use, now attach the dataset locally
attach(dataS)

sex <- sex_name

# Generate a variable ("observed") indicating whether or not the unit is observed (r_i=1)
observed <- 1 - (read == 0)

# Use logistic regression to predict probabilities of being observed
probobs <- glm(observed~(vouch0*sex)+(vouch0*phone),family=binomial(link="logit"))$fitted

# Compare distributions of predicted probabilities across experimental conditions
# Check to make sure that there are no zero predicted probabilities in either condition
summary(probobs[vouch0==0])
summary(probobs[vouch0==1])

# Generate weights: inverse of predicted probability of being observed
wt <- 1/probobs

# Restrict analysis to observed subjects.
sel_valid <- observed == 1
table(sel_valid)

# Coefficients for unweighted regression (restricting analysis to observed subjects)
lm(read~vouch0,subset=sel_valid)$coefficients

# Coefficients for IPW regression (restricting analysis to observed subjects)
lm(read~vouch0,weights=wt,subset=sel_valid)$coefficients

# analysis for Table 7.5
missing <- 1-observed

summary(lm(missing~age+sex+phone,subset=vouch0==0))

summary(lm(missing~age+sex+phone,subset=vouch0==1))