## Code to Reproduce Figure 11.1

## Clear any previous work
rm(list=ls(all=TRUE))

## Set seed using the standard seed for the whole book
set.seed(1234567)

## Generate three normal distributions, using n=100000
prior <- rnorm(1000000, mean = 0, sd = 2)
posterior <- rnorm(1000000, mean = 8, sd = .89)
experimental <- rnorm(100000, mean = 10, sd = 1)

data11 <- cbind(prior,posterior,experimental)
## Draw plot to replicate Figure 11.1

plot(density(experimental, adjust=1.4), xlim=c(-10,17), ylim=c(0,0.45), lty=2, main="Figure 11.1", xlab="")
points(density(prior, adjust=1.4), type="l", lty=3)
points(density(posterior, adjust=1.4), type="l")
legend(-7,.1,"Prior", bty="n")
legend(1,.2,"Posterior", bty="n")
legend(11,.2,"Experimental", bty="n")
legend(11,.185,"result", bty="n")
legend(11.5,.155,"mean=10", bty="n", cex=0.7)
legend(11.5,.145,"sd=1", bty="n", cex=0.7)
legend(1.5,.175,"mean=8", bty="n", cex=0.7)
legend(1.5,.165,"sd=0.89", bty="n", cex=0.7)
legend(-6.5,.075,"mean=0", bty="n", cex=0.7)
legend(-6.5,.065,"sd=2", bty="n", cex=0.7)

## Save data generated as csv
write(data11, file="ch11figure1data.csv")