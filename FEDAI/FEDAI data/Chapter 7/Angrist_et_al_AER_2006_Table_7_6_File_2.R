# Code for chapter 7: simulation of follow-up strategy
rm(list = ls(all = TRUE))
gc()

manski2 <- function(data1,control.M1,control.M2,control.M2a,treated.M1,treated.M2,treated.M2a)  {

  temp <- rep(NA,11)

  # save true ATE estimate in this sample
  temp[5] <- mean(data1$Y1[data1$Tr == 1]) - mean(data1$Y0[data1$Tr == 0])


  # calculate lower and upper Manski bounds for Y0 (CONTROLS)
  denom.Co <- sum(data1$Tr == 0)

  w1.Co <- sum(data1$Tr == 0 & data1$M1 == 0) / denom.Co                    # weight for fully observed outcomes
  w2.Co <- sum(data1$Tr == 0 & data1$M1 == 1 & data1$M2 == 0) / denom.Co    # weight for outcomes observed during follow-up
  w3.Co <- sum(data1$Tr == 0 & data1$M1 == 1 & data1$M2 == 1) / denom.Co    # weight for outcomes imputed with extreme values

  # consistency check
  stopifnot(abs(w1.Co+w2.Co+w3.Co-1) < .00001)

  m1.Co <- mean(data1$Y[data1$Tr == 0 & data1$M1 == 0])
  m2.Co <- mean(data1$Y[control.M2a & data1$M2 == 0])

  # consistency check
  if(length(data1$Y[control.M2a & data1$M2 == 0]) == 0) m2.Co <- 0

  # lower bound
  temp[3] <- w1.Co*m1.Co + w2.Co*m2.Co + w3.Co*0

  # upper bound
  temp[4] <- w1.Co*m1.Co + w2.Co*m2.Co + w3.Co*1

  # calculate less naive estimator that uses randomization and follow-up outcomes but ignores 2nd stage missingness
  temp[7] <- w1.Co/(w1.Co+w2.Co)*m1.Co + w2.Co/(w1.Co+w2.Co)*m2.Co

  # calculate Manski bounds for naive estimator
  temp[8] <- m1.Co*w1.Co
  temp[9] <- m1.Co*w1.Co + (w2.Co+w3.Co)


  # calculate lower and upper Manski bounds for Y1 (TREATED)
  denom.Tr <- sum(data1$Tr == 1)

  w1.Tr <- sum(data1$Tr == 1 & data1$M1 == 0) / denom.Tr                    # weight for fully observed outcomes
  w2.Tr <- sum(data1$Tr == 1 & data1$M1 == 1 & data1$M2 == 0) / denom.Tr    # weight for outcomes observed during follow-up
  w3.Tr <- sum(data1$Tr == 1 & data1$M1 == 1 & data1$M2 == 1) / denom.Tr    # weight for outcomes imputed with extreme values

  # consistency check
  stopifnot(abs(w1.Tr+w2.Tr+w3.Tr-1) < .00001)

  m1.Tr <- mean(data1$Y[data1$Tr == 1 & data1$M1 == 0])
  m2.Tr <- mean(data1$Y[treated.M2a & data1$M2 == 0])

  # consistency check
  if(length(data1$Y[treated.M2a & data1$M2 == 0]) == 0) m2.Tr <- 0

  # lower bound
  temp[1] <- w1.Tr*m1.Tr + w2.Tr*m2.Tr + w3.Tr*0

  # upper bound
  temp[2] <- w1.Tr*m1.Tr + w2.Tr*m2.Tr + w3.Tr*1

  # calculate less naive estimator that uses randomization and follow-up outcomes but ignores 2nd stage missingness
  temp[6] <- w1.Tr/(w1.Tr+w2.Tr)*m1.Tr + w2.Tr/(w1.Tr+w2.Tr)*m2.Tr

  # calculate Manski bounds for naive estimator
  temp[10] <- m1.Tr*w1.Tr
  temp[11] <- m1.Tr*w1.Tr + (w2.Tr+w3.Tr)


  return(invisible(temp))
  }


# binary outcomes
# 50% of observations randomly assigned to treatment
# attrition depends flexibly on the underlying "type" of the observation, formed by the 2x2 table of
# potential outcomes


Types are:
# "I"   # success only under treatment
# "II"  # success only under control
# "III" # never success
# "IV"  # always success 

# define probabilities of missingness in FIRST round depending on type for CONTROL obs
typeI.p.1.Co <- .5
typeII.p.1.Co <- .5
typeIII.p.1.Co <- .7
typeIV.p.1.Co <- .3

# define probabilities of missingness in SECOND round depending on type for CONTROL obs
typeI.p.2.Co <- .5
typeII.p.2.Co <- .5
typeIII.p.2.Co <- .5
typeIV.p.2.Co <- .5


# define probabilities of missingness in FIRST round depending on type for TREATED obs
typeI.p.1.Tr <- 0
typeII.p.1.Tr <- 0
typeIII.p.1.Tr <- 0
typeIV.p.1.Tr <- 0

# define probabilities of missingness in SECOND round depending on type for TREATED obs
typeI.p.2.Tr <- 0
typeII.p.2.Tr <- 0
typeIII.p.2.Tr <- 0
typeIV.p.2.Tr <- 0


sims <- 5000    # number of simulation runs
n <- 10000      # sample size


y0.prob <- .45  # probability of Y0 == 1
y1.prob <- .55  # probability of Y1 == 1


# number of observations with initial missingness that are chosen for follow-up
follow.up.Co.no <- 400
follow.up.Tr.no <- 400



res <- matrix(NA,sims,14)
colnames(res) <- c( "Avg.Y1.naive","Avg.Y0.naive","Avg.Y1.lower","Avg.Y1.upper",
                    "Avg.Y0.lower","Avg.Y0.upper","ATE","ATE lower", "ATE upper",
                    "Avg.Y1.lessnaive","Avg.Y0.lessnaive", "ATE.lessnaive",
                    "Lower ATE bound naive","Upper ATE bound naive")

m <- 1
for(m in 1:sims)    {

    # create schedule of potential outcomes
    data1 <- matrix(NA,n,6)
    colnames(data1) <- c("Tr","Y0","Y1","M1","M2","Y")
    data1 <- as.data.frame(data1)

    # generate treatment assignments
    data1$Tr <- rbinom(n,1,.5)

    # generate potential outcomes
    data1$Y0 <- rbinom(n,1,y0.prob)
    data1$Y1 <- rbinom(n,1,y1.prob)

    # record type of each observation
    type <- rep(NA,n)
    type[data1$Y0 == 0 & data1$Y1 == 1] <- "I"      # success only under treatment
    type[data1$Y0 == 1 & data1$Y1 == 0] <- "II"     # success only under control
    type[data1$Y0 == 0 & data1$Y1 == 0] <- "III"    # never success
    type[data1$Y0 == 1 & data1$Y1 == 1] <- "IV"     # always success
table(type)

    # generate missingness indicator for first stage as a function of treatment assignment and type
    data1$M1[data1$Tr == 0 & type == "I"] <-
    rbinom(n,1,typeI.p.1.Co)[data1$Tr == 0 & type == "I"]
    data1$M1[data1$Tr == 0 & type == "II"] <-
    rbinom(n,1,typeII.p.1.Co)[data1$Tr == 0 & type == "II"]
    data1$M1[data1$Tr == 0 & type == "III"] <-
    rbinom(n,1,typeIII.p.1.Co)[data1$Tr == 0 & type == "III"]
    data1$M1[data1$Tr == 0 & type == "IV"] <-
    rbinom(n,1,typeIV.p.1.Co)[data1$Tr == 0 & type == "IV"]

    data1$M1[data1$Tr == 1 & type == "I"] <-
    rbinom(n,1,typeI.p.1.Tr)[data1$Tr == 1 & type == "I"]
    data1$M1[data1$Tr == 1 & type == "II"] <-
    rbinom(n,1,typeII.p.1.Tr)[data1$Tr == 1 & type == "II"]
    data1$M1[data1$Tr == 1 & type == "III"] <-
    rbinom(n,1,typeIII.p.1.Tr)[data1$Tr == 1 & type == "III"]
    data1$M1[data1$Tr == 1 & type == "IV"] <-
    rbinom(n,1,typeIV.p.1.Tr)[data1$Tr == 1 & type == "IV"]


    # generate missingness indicator for second stage as a function of treatment assignment and type
    data1$M2[data1$Tr == 0 & type == "I" & data1$M1 == 1] <-
    rbinom(n,1,typeI.p.2.Co)[data1$Tr == 0 & type == "I" & data1$M1 == 1]
    data1$M2[data1$Tr == 0 & type == "II" & data1$M1 == 1] <-
    rbinom(n,1,typeII.p.2.Co)[data1$Tr == 0 & type == "II" & data1$M1 == 1]
    data1$M2[data1$Tr == 0 & type == "III" & data1$M1 == 1] <-
    rbinom(n,1,typeIII.p.2.Co)[data1$Tr == 0 & type == "III" & data1$M1 == 1]
    data1$M2[data1$Tr == 0 & type == "IV" & data1$M1 == 1] <-
    rbinom(n,1,typeIV.p.2.Co)[data1$Tr == 0 & type == "IV" & data1$M1 == 1]

    data1$M2[data1$Tr == 1 & type == "I" & data1$M1 == 1] <-
    rbinom(n,1,typeI.p.2.Tr)[data1$Tr == 1 & type == "I" & data1$M1 == 1]
    data1$M2[data1$Tr == 1 & type == "II" & data1$M1 == 1] <-
    rbinom(n,1,typeII.p.2.Tr)[data1$Tr == 1 & type == "II" & data1$M1 == 1]
    data1$M2[data1$Tr == 1 & type == "III" & data1$M1 == 1] <-
    rbinom(n,1,typeIII.p.2.Tr)[data1$Tr == 1 & type == "III" & data1$M1 == 1]
    data1$M2[data1$Tr == 1 & type == "IV" & data1$M1 == 1] <-
    rbinom(n,1,typeIV.p.2.Tr)[data1$Tr == 1 & type == "IV" & data1$M1 == 1]

    data1$M2[data1$M1 == 0] <- 0    # obs not missing in first round are not missing in follow-up


    # determine observed Y
    data1$Y[data1$Tr == 1 & data1$M1 == 0] <- data1$Y1[data1$Tr == 1 & data1$M1 == 0]
    data1$Y[data1$Tr == 0 & data1$M1 == 0] <- data1$Y0[data1$Tr == 0 & data1$M1 == 0]

    # naively calculate average observed outcomes, discarding missing observations
    res[m,1] <- mean(data1$Y[data1$Tr == 1], na.rm = TRUE)
    res[m,2] <- mean(data1$Y[data1$Tr == 0], na.rm = TRUE)


    # randomly choose fraction of obs with initial missingness for detailed follow-up
    control.M1  <- which(data1$Tr == 0 & data1$M1 == 1)

    follow.up.Co <- follow.up.Co.no / length(control.M1)
    if(follow.up.Co > 1) follow.up.Co <- 1

    control.M2  <- sample(control.M1, floor(length(control.M1)*follow.up.Co), replace = FALSE)
    control.M2a <- is.element(1:n, control.M2)

    data1$Y[control.M2a & data1$M2 == 0] <- data1$Y0[control.M2a & data1$M2 == 0]


    treated.M1  <- which(data1$Tr == 1 & data1$M1 == 1)

    follow.up.Tr <- follow.up.Tr.no / length(treated.M1)
    if(follow.up.Tr > 1) follow.up.Tr <- 1

    treated.M2  <- sample(treated.M1, floor(length(treated.M1)*follow.up.Tr), replace = FALSE)
    treated.M2a <- is.element(1:n, treated.M2)

    data1$Y[treated.M2a & data1$M2 == 0] <- data1$Y1[treated.M2a & data1$M2 == 0]

    # consistency check:
    stopifnot(sum(is.na(data1[,1:5])) == 0)


    # calculate Manski bounds
    temp <- manski2(data1 = data1,
    control.M1 = control.M1, control.M2 = control.M2, control.M2a = control.M2a,
    treated.M1 = treated.M1, treated.M2 = treated.M2, treated.M2a = treated.M2a)

    res[m,3:7] <- temp[1:5]
    res[m,10:11] <- temp[6:7]

    # lower and upper bounds on ATE
    res[m,8] <- res[m,3] - res[m,6]
    res[m,9] <- res[m,4] - res[m,5]
    res[m,12] <- res[m,10] - res[m,11]

    # lower and upper bounds on naive ATE
    res[m,13] <- temp[10] - temp[9]
    res[m,14] <- temp[11] - temp[8]

    cat(m,"\n")


    }

# consistency check
stopifnot(sum(is.na(res)) == 0)


# naive estimator
#round(mean(res[,1] - res[,2]),4)
#round(sd(res[,1] - res[,2]),4)

# BOUNDS naive estimator
round(mean(res[,13]),4)
round(sd(res[,13]),4)
round(mean(res[,14]),4)
round(sd(res[,14]),4)








# less naive estimator
round(mean(res[,12]),4)
round(sd(res[,12]),4)

# new estimator (without 2nd stage missingness)
#round(mean(res[,9]),4)
#round(sd(res[,9]),4)

# new estimator (with 2nd stage missingness; lower bound on ATE)
round(mean(res[,8]),4)
round(sd(res[,8]),4)

# new estimator (with 2nd stage missingness; upper bound on ATE)
round(mean(res[,9]),4)
round(sd(res[,9]),4)

# new estimator (with 2nd stage missingness; width of bound on ATE)
round(mean(res[,9] - res[,8]),4)
round(sd(res[,9] - res[,8]),4)



# graph results
hist(res[,8], col = "blue", 100, xlim = c(min(c(res[,8],res[,9])), max(c(res[,8],res[,9]))))
hist(res[,9], col = "green", 100, add = TRUE)

abline(v = .05, col = "red", lwd = 1.5)



# consistency check: true effect bracketed by bounds
table(res[,8] <= res[,7])
table(res[,9] >= res[,7])
