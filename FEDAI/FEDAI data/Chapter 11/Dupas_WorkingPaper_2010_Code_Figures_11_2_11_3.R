# Creating Figures 11.2 and 11.3

#Clear any previous work
rm(list=ls(all=TRUE))

#Load Relevant packages
library(foreign)

#Read in Data from Internet
bedout <- read.dta("http://hdl.handle.net/10079/xksn0db")

attach(bedout)

quartz(width=8,height=4.5)

par(family="Gill Sans MT",font.main=1,mfrow = c(1, 2),pty = "s")

plot(purchaserate~price,pch=16,ylab="Purchase Rate",xlab="Price",xlim=c(0,300),ylim=c(0,1))

summary(lm(purchaserate~price))

lines(predict(lm(purchaserate~price),data.frame(price=c(-10:400)))~c(-10:400))

text("R-squared = 0.897",x=225,y=.9)

plot(logitr~price,pch=16,ylab="logit(Purchase Rate)",xlab="Price",xlim=c(0,300),ylim=c(-2,2))

summary(lm(logitr~price))

lines(predict(lm(logitr~price),data.frame(price=c(-10:400)))~c(-10:400))

text("R-squared = 0.925",x=225,y=1.6)



logprice <- log(price)

plot(purchaserate~price,pch=16,ylab="Purchase Rate",xlab="Price (Logarithmic Scale)",xlim=c(20,500),ylim=c(0,1), log="x")

summary(lm(purchaserate~logprice))

lines(predict(lm(purchaserate~logprice),data.frame(logprice=c(1:100)))~exp(1:100))

text("R-squared = 0.955",x=225,y=.9)

plot(logitr~price,pch=16,ylab="logit(Purchase Rate)",xlab="Price (Logarithmic Scale)",xlim=c(20,500),ylim=c(-2,2), log="x")

summary(lm(logitr~logprice))

lines(predict(lm(logitr~logprice),data.frame(logprice=c(1:400)))~exp(c(1:400)))

text("R-squared = 0.968",x=225,y=1.6)

# add fitted line, R^2