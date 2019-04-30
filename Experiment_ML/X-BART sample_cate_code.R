if (!require("devtools")) install.packages("devtools")
devtools::install_github("soerenkuenzel/hte")

library(hte)
library(readstata13)
library(dplyr)

data <- read.dta13("~/Dropbox/Texting_Voter_Turnout_Field_Experiments_2019/NGA IL 2016 General Experiment/Relay SMS Test/data/nextgen_relay_sms_clean_vh.dta")

# randomly sample 1k observations for convenience
data <- sample_n(data, 1000)

# list of covariates on which to train the model
# making this a small list for convenience
covars <- c("general14", "general12", "general10", "general08",
            "vf_female", "vf_white", "tsmpart")

# create a matrix with these covars
X <- data[, covars]

# create a vector of treatment
Tr <- data[, "treat_binary"]

# create a vector of outcomes
yobs <- data[, "voted"]

# Use BART to build a model
xl_bart <- X_BART(feat = X, 
                  tr = Tr, 
                  yobs = yobs)

# estimate the CATE
data$cate <- EstimateCate(xl_bart, X)

# examine CATE
mean(data$cate)
sd(data$cate)
hist(data$cate)