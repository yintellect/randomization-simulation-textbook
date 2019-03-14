rm(list = ls())
suppressMessages({
        library(foreign)
        library(arm)
        library(dplyr)
        library(knitr)
})

setwd("~/MA/2019Spring/RAship/FEDAI/FEDAI data/Chapter 11")

dupas <- read.dta("Chapter 11_Dupas (2010) Dataset.dta")
dupas <- within(dupas,{
        purchased <- as.numeric(purchasednet =="yes")
        log_price <- log(price)
        region <- cfw_id})


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
kable(table_11_3_mod[,c(1:4, 5:7)],caption = "Modified Table 11-3 (Model 1)")
kable(table_11_3_mod[,c(1:4, 8:10)],caption = "Modified Table 11-3 (Model 2)")


model_1_chi_sq <- with(table_11_3_mod, sum(chi_square_1))
pvalue_1 = pchisq(model_1_chi_sq, 9, lower.tail = FALSE)


model_2_chi_sq <- with(table_11_3_mod, sum(chi_square_2))
pvalue_2 = pchisq(model_2_chi_sq, 13, lower.tail = FALSE)



model_1_chi_sq
pvalue_1
model_2_chi_sq
pvalue_2
