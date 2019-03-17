# program to create boxes 5.4, 5.5, and 5.6 using New Haven data

#Clear any previous work
rm(list=ls(all=TRUE))

#Load Relevant packages
library(AER)
library(sandwich)
setwd("~/MA/2019Spring/RAship/FEDAI/exmaple_in_FEIDA")
data1 <- read.csv(file="Gerber_Green_APSRsubset_2005.csv",head=TRUE,sep=",")

# You can check that your data was read in correctly using these two commands:
# colnames(data1)
# dim(data1)

# select one-person households that were either pure controls or canvass only
sel <-  data1$onetreat==1 & data1$mailings==0 & data1$phongotv==0 & data1$persons==1

# verify the number of observations
table(sel)
data2 <- data1[sel,]

# define variables
v98      <- data2$v98
persngrp <- data2$persngrp
cntany   <- data2$cntany

############  NOTE USE OF ROBUST STANDARD ERRORS


# Box 5.4: ITT
coef(summary(lm(v98 ~ persngrp)))
# robust SEs
itt_fit <- lm(v98 ~ persngrp)
coeftest(itt_fit,vcovHC(itt_fit))
coeftest(itt_fit,vcovHC(itt_fit, type = "HC3"))

# Box 5.5: ITT_D
# Note that results from this will vary based on the current version that you have but this variation should not be a concern. 
coef(summary(lm(cntany ~ persngrp)))
# robust SEs
itt_d_fit <- lm(cntany ~ persngrp)
coeftest(itt_d_fit,vcovHC(itt_d_fit))

coeftest(itt_d_fit,vcovHC(itt_d_fit, type = "HC3"))

# Box 5.6: CACE
coef(summary(ivreg(v98 ~ cntany,~persngrp)))
# robust SEs
cace_fit <- ivreg(v98 ~ cntany,~persngrp)
coeftest(cace_fit,vcovHC(cace_fit, type = "HC3"))


coeftest(cace_fit,vcovHC(cace_fit))


