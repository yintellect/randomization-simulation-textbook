# Exercise 9.7 Bertrand and Mullainathan

rm(list=ls())       # clear objects in memory

# Set your working directory
# setwd("")

# Recreate Dataset
Y <- c(rep(1, 38), rep(0, 542-38), rep(1, 55), rep(0, 542-55), 
       rep(1, 46), rep(0, 541-46), rep(1, 71), rep(0, 541-71), 
       rep(1, 37), rep(0, 670-37), rep(1, 48), rep(0, 670-48),
       rep(1, 36), rep(0, 682-36), rep(1, 61), rep(0, 682-61))

# You could code Boston as 1
boston <- c(rep(1, 542+542+541+541), rep(0, 670+670+682+682))

# ... or You could code Chicago as 1
chicago <- 1-boston

# Low Quality as 1
lowquality <- c(rep(1, 542+542), rep(0, 541+541), rep(1, 670+670), rep(0, 682+682))

# ... or High Quality as 1
highquality <- 1-lowquality

# Black as 1
black<- c(rep(1, 542), rep(0,542), rep(1, 541), rep(0,541), 
          rep(1, 670), rep(0,670), rep(1, 682), rep(0,682))
# .... White as 1.
white <- 1-black


# All the models are
# Y ~ race + quality + city + race*quality + race*city + quality*city + race*quality*city
# In principle, there are 8 possibilities... here are 4.

fit_1 <- lm(Y ~ white + highquality + chicago + white*highquality + 
              white*chicago + highquality*chicago + white*highquality*chicago)
fit_2 <- lm(Y ~ black + highquality + chicago + black*highquality + 
              black*chicago + highquality*chicago + black*highquality*chicago)
fit_3 <- lm(Y ~ white + highquality + boston + white*highquality + 
              white*boston + highquality*boston + white*highquality*boston)
fit_4 <- lm(Y ~ black + lowquality + chicago + black*lowquality + 
              black*chicago + lowquality*chicago + black*lowquality*chicago)