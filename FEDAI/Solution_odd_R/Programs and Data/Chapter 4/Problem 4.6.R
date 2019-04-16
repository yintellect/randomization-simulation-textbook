# Exercise 4.6 (blockTools exampe)

rm(list=ls())       # clear objects in memory
library(ri)         # load the RI package
set.seed(1234567)   # random number seed, so that results are reproducible
library(blockTools) # load a package that automates block random assignment
library(foreign)    # package allows R to read Stata datasets

# These data come from a 3 wave panel study of rural Russians 1995-1996-1997:  See O'Brien, David J., and Valeri V. Patsiorkovski. 1999.  “Russian Village Household Panel Surveys, 1995-1997.” http://hdl.handle.net/1902.2/2816. Inter-university Consortium for Political and Social Research (ICPSR) [Distributor] V1 [Version].

#setwd("/Users/donaldgreen/Dropbox/Field Experimentation Book/Final Code for Vignettes and Problems/Chapter 4/")
dta <- read.dta("Problem 4.6 Russia.dta")

#### Problem 4.6a ####

russia <- within(russia,{
  female <- as.numeric(sexresp6 == "woman")
  class <- relevel(group6,ref="very poor")
  church_member <- as.numeric(memberc6=="yes")
  id <- 1:nrow(russia)
  class_verypoor <- as.numeric(class=="very poor")
  class_poor <- as.numeric(class=="poor")
  class_middle <- as.numeric(class=="middle")
  class_morethanmiddle <- as.numeric(class=="more than middle")
})

fit <- lm(index96 ~ index95 + female + church_member + class, data=russia)
summary(fit)$r.squared

fit.nolag <- lm(index96 ~ female + church_member + class, data=russia)
summary(fit.nolag)$r.squared

#### Problem 4.6b ####

block.out <- block(data = russia, n.tr = 2, 
                   id.vars = "id",algorithm="randGreedy",
                   block.vars = c("female", "church_member", 
                                  "index96","class_verypoor", 
                                  "class_poor", "class_middle"))
assign.out <- assignment(block.out)

# extracting the treatment assignment from blockTools takes some work
# The commands below check to see which ID numbers appear on the 
# list of assign.out's assignment to Treatment 1

russia$Z_blocked <- as.numeric(russia$id %in% 
                                 as.numeric(as.character(
                                   unlist(assign.out$assg[[1]]["Treatment 1"]))))

summary(lm(Z_blocked ~ female + church_member + class + index96, 
                data=russia))

#### Problem 4.6d ####

sims <- 10000
results <- matrix(NA,sims,3)
colnames(results) <- c("complete","adjusted","blocked")
N <- nrow(russia)

for(i in 1:sims) {
  # Complete RA, with and without adjustment
  russia$Z_complete <- ifelse(1:N %in% sample(N, N/2), 1, 0)
  results[i,1] <- lm(index97 ~ Z_complete, data=russia)$coefficients[2]
  results[i,2] <- lm(index97 ~ Z_complete + female + church_member + class + index96,
                     data=russia)$coefficients[2]
  
  # Blocked RA, without adjustment
  assign.out <- assignment(block.out)
  russia$Z_blocked <- as.numeric(russia$id %in% 
                                   as.numeric(as.character(
                                     unlist(assign.out$assg[[1]]["Treatment 1"]))))
  results[i,3] <- lm(index97 ~ Z_blocked, data=russia)$coefficients[2]
}

# use apply() to extract means and SDs for each column (2 refers to columns)
results_table <- rbind(apply(results,2,mean),apply(results,2,sd))
rownames(results_table) <- c("Average Estimate", "Standard Error")

library(xtable)
results_forxtable <- xtable(results_table,caption="Comparison of 3 estimators")
print.xtable(results_forxtable,caption.placement="top",table.placement="H")





# COMPARE SAMPLING DISTRIBUTIONS
sims <- 10000
res <- matrix(NA,sims,3)      # initialize a matrix to store results
colnames(res) <- c("simple","adjusted","blocked")

# use blockTools to implement blocked randomization with two observations in each block, one of which will be assigned to treatment
# use the randGreedy algorithm so that the original order of the observations is irrelevant for the selection of pairs
block.out <- block( data = dta, n.tr = 2, id.vars = "id",algorithm="randGreedy",
                    block.vars = c("female6","group61","group62","group63","index96"))


# generate random assignments of the treatment varible Tr
for(m in 1:sims)    {

    # simple randomization & difference-in-means estimation
    Tr <- sample(c(rep(FALSE,nrow(dta)/2), rep(TRUE, nrow(dta)/2)))
    res[m,1] <- mean(dta$index97[Tr == TRUE]) - mean(dta$index97[Tr == FALSE])

    # covariate adjustment: linear and additive specification
    temp <- lm(index97 ~ Tr + index96 + female6 + group61 + group62 + group63, data = dta)
	# here's an alternative specification: more flexible
	#    temp <- lm(index97 ~ Tr + factor(index96) * female6 * group61 * group62 * group63, data = dta)

    res[m,2] <- coefficients(temp)[2]   # store the coefficient for Tr

    # blocked randomization & difference-in-means estimation
    assign.out <- assignment(block.out)    # blockTools' automated assignment
    # extracting Tr from the assign.out$assg list takes some work
    # the is.element test checks to see which ID numbers appear on the list of assign.out's assignment to Treatment 1
    Tr <- is.element(1:length(Tr), as.numeric(as.character(unlist(assign.out$assg[[1]]["Treatment 1"]))))
    res[m,3] <- mean(dta$index97[Tr == TRUE]) - mean(dta$index97[Tr == FALSE])

    }


# results 
# use apply() to extract means and SDs for each column (2 refers to columns)
round(apply(res,2,mean),3)
round(apply(res,2,sd),3)

