library(e1071)

set.seed(1234)

Nunits <- 8
Nperiods <- 3

exposmat <- rbind(
c(1,11,11),
c(1,11,11),
c(0,1,11),
c(0,1,11),
c(0,0,1),
c(0,0,1),
c(0,0,0),
c(0,0,0)
)

unitno <- matrix(c(1:Nunits),ncol=Nperiods,nrow=Nunits,byrow=FALSE)
periodno <- matrix(c(1:Nperiods),ncol=Nperiods,nrow=Nunits,byrow=TRUE)

Y00 <- matrix(
c(4,5,3,
7,5,4,
1,2,4,
4,3,2,
3,3,3,
8,4,3,
2,3,4,
3,1,2)
,nrow=8,ncol=3,byrow=TRUE)

Y01 <- matrix(
c(7,9,4,
8,7,7,
1,2,8,
4,7,10,
4,3,2,
10,6,9,
2,7,6,
5,1,2)
,nrow=8,ncol=3,byrow=TRUE)

Y11 <- matrix(
c(-99,9,4,
-99,8,7,
-99,2,10,
-99,8,10,
-99,3,2,
-99,8,10,
-99,7,6,
-99,2,3)
,nrow=8,ncol=3,byrow=TRUE)



exposmatA <- exposmat[sample(1:Nunits),]

Y <- Y00*(exposmatA==0) + Y01*(exposmatA == 01) + Y11*(exposmatA == 11) 

## Super Naive

mean(Y[exposmatA>0])-mean(Y[exposmatA==0])

# No Lagged

weighted.mean(Y[exposmatA>0],1/(1-prob00[exposmatA>0])) - weighted.mean(Y00[exposmatA==00],1/prob00[exposmatA==00])

# Naive Lagged

mean(Y[exposmatA == 01]) - mean(Y[exposmatA == 0])
mean(Y[exposmatA == 11]) - mean(Y[exposmatA == 0])

## probmat - analytical

prob00 <- matrix(c(.75,.5,.25),nrow=8,ncol=3,byrow=TRUE)
prob01 <- matrix(c(.25,.25,.25),nrow=8,ncol=3,byrow=TRUE)
prob11 <- matrix(c(0,.25,.5),nrow=8,ncol=3,byrow=TRUE)

# All units can be in 00 vs. 01

weighted.mean(Y01[exposmatA==01],1/prob01[exposmatA==01]) - weighted.mean(Y00[exposmatA==00],1/prob00[exposmatA==00])

# Only periods 2 and 3 can be in 00 vs. 11

weighted.mean(Y11[exposmatA==11 & periodno > 1],1/prob11[exposmatA==11 & periodno > 1]) - weighted.mean(Y00[exposmatA==00 & periodno > 1],1/prob00[exposmatA==00 & periodno > 1])

####### compute all randomizations

rands <- permutations(8)

rands[rands == 2] <- 1
rands[rands == 4] <- 3
rands[rands == 6] <- 5
rands[rands == 8] <- 7

rands <- unique(rands)

# 2520 randomizations

computedists <- function(x) {
	exposmatA <- exposmat[x,]
	
	output <- rep(NA,6)
	
	output[1] <- mean(Y[exposmatA>0])-mean(Y[exposmatA==0])

	output[2] <- weighted.mean(Y[exposmatA>0],1/(1-prob00[exposmatA>0])) - weighted.mean(Y00[exposmatA==00],1/prob00[exposmatA==00])

	output[3] <- mean(Y[exposmatA == 01]) - mean(Y[exposmatA == 0])

	output[4] <- mean(Y[exposmatA == 11]) - mean(Y[exposmatA == 0])
	
# All units can be in 00 vs. 01	
	
	output[5] <- weighted.mean(Y01[exposmatA==01],1/prob01[exposmatA==01]) - weighted.mean(Y00[exposmatA==00],1/prob00[exposmatA==00])

# Only periods 2 and 3 can be in 00 vs. 01 vs. 11

	output[6] <- weighted.mean(Y11[exposmatA==11 & periodno > 1],1/prob11[exposmatA==11 & periodno > 1]) - weighted.mean(Y00[exposmatA==00 & periodno > 1],1/prob00[exposmatA==00 & periodno > 1])

	return(output)
	}
	
#### True
	
outputmat <- t(apply(rands,1,computedists))

colMeans(outputmat)
apply(outputmat,2,function(x) mean((x-mean(x))^2)^.5)

#### Rosenbaum-Style Confidence Intervals

tau01est <- weighted.mean(Y01[exposmatA==01],1/prob01[exposmatA==01]) - weighted.mean(Y00[exposmatA==00],1/prob00[exposmatA==00])

tau11est <- weighted.mean(Y11[exposmatA==11 & periodno > 1],1/prob11[exposmatA==11 & periodno > 1]) - weighted.mean(Y00[exposmatA==00 & periodno > 1],1/prob00[exposmatA==00 & periodno > 1])

Y00est <- Y01est <- Y11est <- Y

Y00est[exposmatA==01] <- Y[exposmatA==01] - tau01est
Y00est[exposmatA==11 & periodno > 1] <- Y[exposmatA==11 & periodno > 1] - tau11est

Y01est[exposmatA==00] <- Y[exposmatA==00] + tau01est
Y01est[exposmatA==11 & periodno > 1] <- Y[exposmatA==11 & periodno > 1] - tau11est + tau01est

Y11est[periodno == 1] <- NA
Y11est[exposmatA==00 & periodno > 1] <- Y[exposmatA==00 & periodno > 1] + tau11est
Y11est[exposmatA==01 & periodno > 1] <- Y[exposmatA==01 & periodno > 1] - tau01est + tau11est

computedistsRos <- function(x) {
	exposmatA <- exposmat[x,]
	
	output <- rep(NA,2)
	
# All units can be in 00 vs. 01	
	
	output[1] <- weighted.mean(Y01est[exposmatA==01],1/prob01[exposmatA==01]) - weighted.mean(Y00est[exposmatA==00],1/prob00[exposmatA==00])

# Only periods 2 and 3 can be in 00 vs. 01 vs. 11

	output[2] <- weighted.mean(Y11est[exposmatA==11 & periodno > 1],1/prob11[exposmatA==11 & periodno > 1]) - weighted.mean(Y00est[exposmatA==00 & periodno > 1],1/prob00[exposmatA==00 & periodno > 1])

	return(output)
	}
	
outputmatRos <- t(apply(rands,1,computedistsRos))

colMeans(outputmatRos)
apply(outputmatRos,2,function(x) mean((x-mean(x))^2)^.5)
t(apply(outputmatRos,2,function(x) quantile(x,c(0.025,0.975))))
