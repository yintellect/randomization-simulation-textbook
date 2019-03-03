setwd("~/16Spring/4368 Experimental/4.FEDAI replication/Chapter 9")
library(foreign)
rm(list = ls())
expand_data<- read.dta("Fieldhouse_et_al_unpublished_2010_expanded.dta")

names(expand_data)
library(dplyr)

problem9 <- rename(expand_data, 
                   mail=m, 
                   phone_assign=p,
                   phone_contact=c,
                   vote=y,
                   phone_contact.mail=c_m,
                   phone_assign.mail=p_m)

ols1 <- lm(phone_contact ~ mail * phone_assign,data=problem9)

ols2 <- lm(phone_contact.mail~ mail * phone_assign, data = problem9)

ols3 <- lm(vote ~ phone_contact*mail, data = problem9)

library(AER)

tslsmodel <- ivreg(vote  ~ mail*phone_contact | mail * phone_assign,data = problem9)
coeftest(tslsmodel,vcovHC(tslsmodel))

