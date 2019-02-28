#  program to illustrate individual values plot and avplot 
#  for chapter on reporting  standards

# Clear Memory in R
rm(list=ls(all=TRUE))

# Load the foreign library to read in stata data files
library(foreign)

Term <- read.dta("Chapter 13_Titiunik (2010) Dataset.dta")

attach(Term)

Term2003 <- subset(Term, year==2003)
attach(Term2003)
as.data.frame(Term2003)
write.csv(Term2003,"Titiunik2010ch13year2003.csv")
Term2003 <- read.csv(file="Titiunik2010ch13year2003.csv", header=TRUE)

Z_alpha <- dshort_term

Z <- as.numeric(dshort_term=="4 years")

#Let's rename three variables
X1 <-  party   
X2 <-  usrepvotesh_dem 
X3 <-  votesh_incumb

Y <- bills_intro

# calculate apparent ATE

mean(Y[Z==1])-mean(Y[Z==0])

# draw individual values plot but with axes switched 
## this is 13.1 graph
set.seed(123456)
plot(Y~jitter(Z,factor=0.5),main="Distributions under Treatment and Control",ylab="Number of Bills Introduced by State Senator",yaxp=c(0,150,5),ylim=c(0,150),xlab="Treatment Condition",xaxp=c(0,1,1),xlim=c(-0.5,1.5), xaxt="n")
axis(1, labels=c("2-Year Term", "4-Year Term"), at=c(0,1))

## Draw Mean lines and Legend for Mean and SD
SDy1 <- sd(Y[Z==1])
SDy0 <- sd(Y[Z==0])
SDy11 <- round(SDy1,2)
SDy00 <- round(SDy0,2)
m1 <- mean(Y[Z==1])
m0 <- mean(Y[Z==0])
m11 <- round(m1, 2)
m00 <- round(m0, 2)
segments(.85,m1,1.15,m1,lwd=2)#, col="grey80")
segments(-.15,m0,0.15,m0,lwd=2)#, col="grey80")

segments(1,y0=m1-sd((Y[Z==1])),y1=m1+sd((Y[Z==1])),lwd=2)#, col="grey80")
segments(0,y0=m0-sd((Y[Z==0])),y1=m0+sd((Y[Z==0])),lwd=2)#, col="grey80")

segments(.985,y0=m1+sd((Y[Z==1])),1.015,y1=m1+sd((Y[Z==1])),lwd=2)#, col="grey80")
segments(.985,y0=m1-sd((Y[Z==1])),1.015,y1=m1-sd((Y[Z==1])),lwd=2)#, col="grey80")

segments(-.015,y0=m0+sd((Y[Z==0])),.015,y1=m0+sd((Y[Z==0])),lwd=2)#, col="grey80")
segments(-.015,y0=m0-sd((Y[Z==0])),.015,y1=m0-sd((Y[Z==0])),lwd=2)#, col="grey80")

#segments(0.325,y0=m0+sd((Y[Z==0])),0.275,y1=m0+sd((Y[Z==0])),lwd=3)#, col="grey80")
#segments(0.325,y0=m0,0.275,y1=m0,lwd=3)#, col="grey80")


#points(m0,-.1, col="grey80", pch=16)
#points(m1,1.1, col="grey80", pch=16)
legend(-.6,62,legend=paste("mean=",m00), bty="n")
legend(-.6,56,legend=paste("sd=",SDy00), bty="n")
legend(1.05,81,legend=paste("mean=",m11), bty="n")
legend(1.05,75,legend=paste("sd=",SDy11), bty="n")

# insert added variables plot (13.2)

library(ri)
X <- cbind(party,usrepvotesh_dem,votesh_incumb)
Z <- as.integer(dshort_term)
resresplot(bills_intro, Z, X,prob=rep(.5,length(bills_intro)), scale=7)



# equivalent Stata code

* program to illustrate individual values plot and avplot for chapter on reporting standards

* replicate Rocio's Table 5
bys  dshort_term: summ  bills_intro

* indiv values plot with a bit of jitter
twoway (scatter bills_intro dshort_term, mfcolor(none) mlcolor(black) jitter(1.5) text(76.875 0 "--", si(huge) c(red))  text(60.13333  1 "--", si(huge) c(red)) xlabel(0 "4 Year Term" 1 "2 Year Term") xscale(range(-.5 1.5)) xtit("") b2(Dash bars represent the mean, size(small)) ylabel(, nogrid))

reg bills_intro dshort_t

reg bills_intro dshort_t party   usrepvotesh_dem votesh_incumb

* added variable plot
keep if year==2003
avplot ds, ytitle("Residuals from a Regression of Outcome on Covariates") ylabel(,nogrid) xtitle("Residuals from a Regression of Treatment on Covariates") note("")

##convert dta file to csv:
