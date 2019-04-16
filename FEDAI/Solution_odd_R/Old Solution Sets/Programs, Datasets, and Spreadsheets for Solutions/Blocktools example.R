# Blocktools example
library(foreign)
library(blockTools)
library(ri)

dta <- read.dta("russia2.dta")
dim(dta)
colnames(dta)

# recode categorical covariates into sets of dummy variables
dta$female6 <- dta$sexresp6 == "woman"

dta$group61 <- dta$group6 == "poor"
dta$group62 <- dta$group6 == "middle"
dta$group63 <- dta$group6 == "more than middle"

dta$memberc6 <- dta$memberc6 == "yes"

dta$id <- 1:nrow(dta)


# which covariates are predictive of the outcome in 1996?
lm.out <- lm(index96 ~ index95 + female6 + group61 + group62 + group63, data = dta)
summary(lm.out)


# use blockTools to implement blocked randomization with four observations in each block
block.out <- block( data = dta, n.tr = 4, id.vars = "id",
                    block.vars = c("female6","group61","group62","group63","index96"))


# randomly assign observations to Tr and Co within blocks
assign.out <- assignment(block.out)



# COMPARE SAMPLING DISTRIBUTIONS
sims <- 10000

res <- matrix(NA,sims,3)
colnames(res) <- c("simple","adjusted","blocked")

# use blockTools to implement blocked randomization with two observations in each block
block.out <- block( data = dta, n.tr = 2, id.vars = "id",
                    block.vars = c("female6","group61","group62","group63","index96"))


set.seed(123)
for(m in 1:sims)    {

    # simple randomization
    Tr <- sample(c(rep(FALSE,nrow(dta)/2), rep(TRUE, nrow(dta)/2)))
    res[m,1] <- mean(dta$index97[Tr == TRUE]) - mean(dta$index97[Tr == FALSE])

    # covariate adjustment
    temp <- lm(index97 ~ Tr + index96 + female6 + group61 + group62 + group63 + memberc6, data = dta)
    res[m,2] <- coefficients(temp)[2]

    # blocked randomization
    assign.out <- assignment(block.out)

    Tr <- is.element(1:length(Tr), as.numeric(as.character(unlist(assign.out$assg[[1]]["Treatment 1"]))))
    res[m,3] <- mean(dta$index97[Tr == TRUE]) - mean(dta$index97[Tr == FALSE])

    }


# results
round(apply(res,2,mean),3)
round(apply(res,2,sd),3)

