# Exercise 11.8 Bed Nets

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(foreign)    # package allows R to read Stata datasets
library(arm)        # for invlogit()
library(dplyr)      # for data manipulation

# Set your working directory
# setwd("")


#### Problem 11.8a ####

table_11_3 <- read.csv("Problem 11.8 Table 11-3.csv", stringsAsFactors = FALSE)

# Model 1
model_1_X2 <- with(table_11_3, tapply(as.numeric(M1_X2), INDEX = Region, FUN = sum, na.rm=TRUE))

# Model 2
model_2_X2 <- with(table_11_3, tapply(as.numeric(M2_X2), INDEX = Region, FUN = sum, na.rm=TRUE))

model_1_X2

model_2_X2



#### Problem 11.8b ####

dupas <- read.dta("Problem 11.8 Dupas.dta")
dupas <- within(dupas,{
  purchased <- as.numeric(purchasednet =="yes")
  log_price <- log(price)
  region <- cfw_id
})

dupas_subset <- subset(dupas, price !=0 & region!=4)

model_1 <- glm(purchased ~ log_price*region, data = dupas_subset, family = "binomial")
model_2 <- glm(purchased ~ log_price + region, data = dupas_subset, family = "binomial")

dupas_subset$preds_1 <- invlogit(predict(model_1))
dupas_subset$preds_2 <- invlogit(predict(model_2))

table_11_3_mod <- 
  dupas_subset %>%
  group_by(region, price) %>%
  summarize(purchases = sum(purchased),
            non_purchases = sum(purchased==0),
            pred_purchases_1 = sum(preds_1),
            pred_nonpurchases_1 = sum(1-preds_1),
            chi_square_1 = (purchases- pred_purchases_1)^2/ pred_purchases_1 + 
              ( non_purchases- pred_nonpurchases_1)^2/ pred_nonpurchases_1,
            pred_purchases_2 = sum(preds_2),
            pred_nonpurchases_2 = sum(1-preds_2),
            chi_square_2 = (purchases- pred_purchases_2)^2/ pred_purchases_2 + 
              ( non_purchases- pred_nonpurchases_2)^2/ pred_nonpurchases_2)


# degrees of freedom: 19 rows in all, minus 5 intercepts and 5 slopes
model_1_chi_sq <- with(table_11_3_mod, sum(chi_square_1))
pvalue_1 = pchisq(model_1_chi_sq, 9, lower.tail = FALSE)

# degrees of freedom: 19 rows in all, minus 5 intercepts and 1 slope
model_2_chi_sq <- with(table_11_3_mod, sum(chi_square_2))
pvalue_2 = pchisq(model_2_chi_sq, 13, lower.tail = FALSE)

model_1_chi_sq
pvalue_1

model_2_chi_sq
pvalue_2


#### Problem 11.8b ####