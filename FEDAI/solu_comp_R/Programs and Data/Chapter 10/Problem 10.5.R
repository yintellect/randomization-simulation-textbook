# Exercise 10.5

rm(list=ls())       # clear objects in memory

# Recreate dataset

Y <- c(rep(0, 400), rep(1, 0), rep(0, 10), rep(1, 90), 
       rep(0, 300), rep(1, 0), rep(0, 100), rep(1, 100))
Z <- c(rep(0, 500), rep(1, 500))
M <- c(rep(0, 400), rep(0, 0), rep(1, 10), rep(1, 90), 
       rep(0, 300), rep(0, 0), rep(1, 100), rep(1, 100))

# Summarize regression
summary(lm(Y~ Z + M))