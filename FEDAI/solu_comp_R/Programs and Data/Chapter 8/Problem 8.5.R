# Exercise 8.5 

rm(list=ls())       # clear objects in memory

# Reproduce dataset
Z_ind <- c(rep(0, 6217), rep(0, 3316), rep(1, 2949), rep(1, 6377))
Z_zip <- c(rep("none", 6217), rep("half", 3316), rep("half", 2949), rep("all", 6377))
Y <- c(rep(1, 1021), rep(0, 6217-1021),
       rep(1, 526), rep(0, 3316-526),
       rep(1, 620), rep(0, 2949-620),
       rep(1, 1316), rep(0, 6377-1316))

ate.firsthand.half <- 
  mean(Y[Z_ind==1 & Z_zip=="half"]) - 
  mean(Y[Z_ind==0 & Z_zip=="half"])

ate.secondhand.untreated <- 
  mean(Y[Z_ind==0 & Z_zip=="half"]) - 
  mean(Y[Z_ind==0 & Z_zip=="none"])

ate.secondhand.treated <- 
  mean(Y[Z_ind==1 & Z_zip=="all"]) - 
  mean(Y[Z_ind==1 & Z_zip=="half"])

ate.firsthand.half
ate.secondhand.untreated 
ate.secondhand.treated