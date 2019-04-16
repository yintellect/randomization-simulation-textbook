rm(list=ls(all=T))

# replace with syntax to read in data file
data <- matrix(1:5000, nrow=1000, ncol=5)

#--------------------------- START CODE TO SUBSET DATA ------------------------------
# Description: 
#   Given an original data frame with N records and given a set seed, this code
#   randomly draws, with replacement, N records from the original data frame.
#
# Instructions:
# (1) Insert this code AFTER you read in the dataset for a given question.
# (2) The original dataset you read in must be named "data"
# (3) You must replace the value assigned to "uni" using the last 4 #s in your UNI
# (4) You must run all of the code at once
# (5) The output dataset is named "mydata"

uni <- 5000
set.seed(uni)
mydata <- as.data.frame(data[sample(1:nrow(data), nrow(data), replace=T),])

#--------------------------- END CODE TO SUBSET DATA --------------------------------
