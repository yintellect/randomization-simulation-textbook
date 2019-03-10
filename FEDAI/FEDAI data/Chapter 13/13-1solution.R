# Exercise 13.1 (direct mail's effect on referendum voting)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets


#  Data are from Middleton, Joel, and Todd Rogers. 2010. “Defend Oregon’s Voter Guide Program.” Report for the Analyst Institute. 

middleton <- read.dta("Chapter 13_Middleton and Rogers (2010) Dataset.dta")
colnames(middleton)


middleton <-within(middleton,{
                Z <- as.numeric(treatment=="yes")
                Y <- relevant_measures_net
                })

colnames(mail)

restricted_ra <- function(){
        middleton$Z_sim <- ifelse(1:65 %in% sample(1:65, 48), 1, 0)# check condition
        fit <- lm(dem_perf_06 ~ Z_sim, data=middleton) 
        if(abs(coef(fit)[2]) < 0.5){
                return(middleton$Z_sim)
                } # if condition is not met, call restricted_ra again return(restricted_ra())
}

set.seed(1234567)
perms <- genperms.custom(randfun = restricted_ra)
probs <- genprob(perms)
# Restricted randomization changes the probabilities that each unit enters into # Here is the distribution of probabilities:
summary(probs)

ate <- with(middleton, estate(Y=Y, Z=Z, prob = probs))
Ys <- with(middleton, genouts(Y = Y, Z = Z, ate = 0))
distout <- gendist(Ys=Ys, perms=perms, prob=probs)
result <- dispdist(distout, ate=ate)
result$greater.p.value
