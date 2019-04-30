# Exercise 9.9 Fieldhouse

rm(list=ls())       # clear objects in memory
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
library(AER)        # package for 2sls

# Set your working directory
# setwd("")

fieldhouse <- read.dta("Problem 9.9 Fieldhouse.dta")

fieldhouse <- within(fieldhouse,{
  mail <- m
  phone <- p
  phone_contact <- c
  vote <- y
})

fit.iv <- ivreg(vote ~ mail + phone_contact + phone_contact*mail 
                | mail + phone + mail*phone, data=fieldhouse)
summary(fit.iv)