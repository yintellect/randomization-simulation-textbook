
rm(list = ls())

library(foreign)

hotspots <- read.csv("GerberGreenBook_Chapter8_Table_8_4_8_5.csv")

# part b)
true_ate_01 <- with(hotspots, mean(y01[prox500==1]) - mean(y00[prox500==1]))
true_ate_10 <- with(hotspots, mean(y10[prox500==1]) - mean(y00[prox500==1]))
true_ate_11 <- with(hotspots, mean(y11[prox500==1]) - mean(y00[prox500==1]))
true_ate_01
true_ate_10
true_ate_11


hotspots <- within(hotspots,{
        exposure[exposure == 11] <- "11" # Indirect and Direct Treatment 
        exposure[exposure == 10] <- "10" # Indirect Treatment 
        exposure[exposure == 01] <- "01" # Direct Treatment 
        exposure[exposure == 00] <- "00" # Control
        Q <- NA
        Q[exposure == "11"] <- prob11[exposure == "11"]
        Q[exposure == "10"] <- prob10[exposure == "10"]
        Q[exposure == "01"] <- prob01[exposure == "01"]
        Q[exposure == "00"] <- prob00[exposure == "00"]
        # Generate weights
        weights <- 1/Q
})

fit.01 <- lm(y ~ exposure, weights=weights,
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "01")))        

fit.10 <- lm(y ~ exposure, weights=weights,
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "10")))


fit.11 <- lm(y ~ exposure, weights=weights,
             subset(hotspots, prox500 > 0 & exposure %in% c("00", "11")))

stargazer::stargazer(fit.01, fit.10, fit.11, type = "text")


# part c)

rm(list = ls())
library(foreign)
hotspot_nonexp <-read.csv("GerberGreenBook_Chapter8_Exercise_9c.csv")


hotspot_nonexp <- within(hotspot_nonexp,{
        exposure[exposure==10] <- "10"
        exposure[exposure==0] <- "00"
        Q <- NA
        Q[exposure=="10"] <- prob10[exposure=="10"]
        Q[exposure=="00"] <- prob00[exposure=="00"]
        weights <- 1/Q
})

fit.nonexp <- lm(y ~ exposure, weights=weights,
                 data=subset(hotspot_nonexp, prob10 > 0 & prob10 < 1))
fit.nonexp
