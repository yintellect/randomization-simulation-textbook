
#   -----------------------------------------------------------------------
#   P111. collinearity penalty decline as the sample size increases
#         the gains from blocking diminish
#   -----------------------------------------------------------------------

# case 1: 75 Hindu villages and 25 non-Hindu villages
villages <- c(rep(0,25),rep(1,75)) # block variable

# case 2: 750 Hindu villages and 250 non-Hindu villages
# villages <- rep(villages,10)



treatment <- c(rep(1,length(villages)/2),rep(0,length(villages)/2)) #assign treatment


numiter <- 100000

storecorr2 <- storemean <- rep(NA,numiter)

for (iter in 1:numiter)
                       {
						random_draw <- sample(villages,length(villages)/2,replace=FALSE)
                        storemean[iter] <- mean(random_draw)
                        
                        # generate the control group, which is the rest of the villages
   control_ones  <- rep(1,mean(villages)*length(villages) - mean(random_draw)*length(villages)/2)
   control_zeros <- rep(0,length(random_draw) - length(control_ones))
                        control_draw <- c(control_ones,control_zeros)
                        ethnicity <- c(random_draw,control_draw)
                        storecorr2[iter] <- (cor(cbind(ethnicity,treatment)))^2
                        }

storemean <- sort(storemean)
# find 2.5th and 97.5th percentiles

lower <- round(numiter/40,0)
upper <- round(numiter - numiter/40,0)

lower_result <- storemean[lower]
upper_result <- storemean[upper]

# number of replications
numiter
# number of villages in simulation
length(villages)
lower_result
upper_result

storecorr2 <- sort(storecorr2)
mean(storecorr2)