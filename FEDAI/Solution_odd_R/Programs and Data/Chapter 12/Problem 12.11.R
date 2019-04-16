# Exercise 12.11 Vouchers

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets

# Set your working directory
# setwd("")

howell <- read.dta("Problem 12.11 Howell and Peterson.dta")

#### Exercise 12.11a ####
with(howell,prop.table(table(missing_y1math, treat), 2))

#### Exercise 12.11b ####
with(howell, mean(y0_1math_change[treat==0],na.rm=TRUE))
with(howell, mean(y0_1math_change[treat==1],na.rm=TRUE))

#### Exercise 12.11c ####
with(howell, hist(y0_1math_change[treat==1], breaks=50))
with(howell, c(min(y0_1math_change[treat==1], na.rm = TRUE),
               max(y0_1math_change[treat==1], na.rm = TRUE)))
with(howell, quantile(y0_1math_change[treat==1], 
                      probs = c(.05, .10, .15, .25, .50, .75, .85, .90, .95),
                      na.rm=TRUE))

#### Exercise 12.11d ####
with(howell, quantile(y0_1math_change[treat==1], 
                      probs = c(.936),na.rm=TRUE))

#### Exercise 12.11e ####
howell <- within(howell,{
  treat_less_than_40 <- as.numeric(treat == 1 & y0_1math_change <40 & missing_y1math==0)
})

L_B <- with(howell, mean(y0_1math_change[treat==1 & treat_less_than_40==1]))
L_B
with(howell, mean(treat_less_than_40[treat==1]==0))

#### Exercise 12.11f ####
L_B - with(howell, mean(y0_1math_change[treat==0], na.rm=TRUE))

#### Exercise 12.11g ####
with(howell, quantile(y0_1math_change[treat==1], 
                      probs = c(0.064),na.rm=TRUE))

#### Exercise 12.11h ####
howell <- within(howell,{
  treat_greater_than_18 <- as.numeric(treat == 1 & y0_1math_change >-18 & missing_y1math==0)
})

U_B <- with(howell, mean(y0_1math_change[treat==1 & treat_greater_than_18==1]))
U_B
with(howell, mean(treat_greater_than_18[treat==1]==0))

#### Exercise 12.11i ####
U_B - with(howell, mean(y0_1math_change[treat==0], na.rm=TRUE))


