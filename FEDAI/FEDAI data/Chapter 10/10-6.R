rm(list=ls())
condition <- c(rep("No Mail", 2586), rep("Standard", 6858),
               rep("Threat", 6694), rep("Norms", 6825),
               rep("Threat+Norms", 6960), rep("Fairness", 6920),
               rep("Threat+Fairness", 6750))
no_mail <- as.numeric(grepl(pattern="Mail", condition))
standard <- as.numeric(grepl(pattern="Standard", condition))
threat <- as.numeric(grepl(pattern="Threat", condition))
norms <- as.numeric(grepl(pattern="Norms", condition))
fairness <- as.numeric(grepl(pattern="Fairness", condition))
Y <- c(rep(1, round(0.0158*2586)), rep(0, 2586 - round(0.0158*2586)),
       rep(1, round(0.0862*6858)), rep(0, 6858 - round(0.0862*6858)),
       rep(1, round(0.0967*6694)), rep(0, 6694 - round(0.0967*6694)),
       rep(1, round(0.0823*6825)), rep(0, 6825 - round(0.0823*6825)),
       rep(1, round(0.0970*6960)), rep(0, 6960 - round(0.0970*6960)),
       rep(1, round(0.0819*6920)), rep(0, 6920 - round(0.0819*6920)),
       rep(1, round(0.0932*6750)), rep(0, 6750 - round(0.0932*6750)))

fit.1 <- lm(Y~no_mail + standard + threat + norms +
                    fairness + threat:norms + threat:fairness - 1)
summary(fit.1)

fit.3 <- lm(Y~no_mail + standard + threat + norms +
                    fairness + threat:norms + threat:fairness)
summary(fit.3)

sd(no_mail)
sd(standard)
sd(threat)
sd(norms)
sd(fairness)
sd(threat:norms)
