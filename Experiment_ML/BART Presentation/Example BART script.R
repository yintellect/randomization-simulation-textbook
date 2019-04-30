### Artificial example to illustrate tree models
### Using R 2.13.0
library(tree)
library(BayesTree)

rm(list = ls(all = TRUE))
#setwd("C:/Users/Holger/Documents/Dropbox/Current projects/Machine Learning/")


#windows()

set.seed(153)

n <- 200
Tr <- rbinom(n,1,.5)
x <- rnorm(n,4,1)
y0 <- rnorm(n,x^2,4)
y1 <- y0 + Tr*x^2 + rnorm(n,0,2)
y <- y0
y[Tr==1] <- y1[Tr==1]


dat <- data.frame(y,Tr,x)
temp <- lm(y ~ Tr*x, data = dat, x = TRUE, model = TRUE)
summary(temp)


# true response surfaces
y0.true <- matrix(NA,1000,2)
y0.true[,1] <- seq(from = min(x), to = max(x), length.out = 1000)
y0.true[,2] <- y0.true[,1]^2

y1.true <- matrix(NA,1000,2)
y1.true[,1] <- seq(from = min(x), to = max(x), length.out = 1000)
y1.true[,2] <- y0.true[,2] + y1.true[,1]^2


# OLS fit
dat <- data.frame(0, seq(from = min(x), to = max(x), length.out = 1000))
colnames(dat) <- c("Tr","x")
pred <- predict(temp, newdata = dat)
 #lines(x = y1.true[,1], y = pred, lty = 2, lwd = 1.5)

dat <- data.frame(1, seq(from = min(x), to = max(x), length.out = 1000))
colnames(dat) <- c("Tr","x")
pred <- predict(temp, newdata = dat)
# lines(x = y1.true[,1], y = pred, lty = 2, lwd = 1.5)


# BART fit
temp.X <- data.frame(Tr,x)
temp.S <- data.frame(c(rep(1,n),rep(0,n)), c(sort(x),sort(x)))
colnames(temp.S) <- colnames(temp.X)

out.bart <- bart(x.train = temp.X, y.train = y, ndpost = 1000, nskip = 1000, keepevery = 1,
                ntree = 200,
                usequants = TRUE, keeptrainfits = FALSE, x.test = temp.S)

cat(dim(out.bart$yhat.test), "\n")


out.bart0 <- out.bart
out.bart1 <- out.bart

out.bart1$yhat.test <- out.bart$yhat.test[,1:(ncol(out.bart$yhat.test)/2)]
out.bart0$yhat.test <- out.bart$yhat.test[,((ncol(out.bart$yhat.test)/2)+1):ncol(out.bart$yhat.test)]


out <- matrix(NA,n,3)
out[,1] <- sort(x)
colnames(out) <- c("x value", "Y0", "Y1")

for(i in 1:n)   {
    out[i,2] <- mean(out.bart0$yhat.test[,i])
    out[i,3] <- mean(out.bart1$yhat.test[,i])
                }


# Add single tree fit
temp.tree <- tree(y ~ Tr + x, mincut = 25)
summary(temp.tree)
plot(temp.tree)
temp.tree


# plot
plot(x[Tr == 0], y[Tr == 0], ylim = range(y), xlim = range(x), ylab = "Y", xlab = "X", pch = 21,
bg = "white", cex = 1)
points(x[Tr == 1], y[Tr == 1], col = "black", pch = 24, bg = "white", cex = .8)

lines(x = y0.true[,1], y = y0.true[,2], lty = 1, lwd = 2)
lines(x = y1.true[,1], y = y1.true[,2], lty = 1, lwd = 2)

lines(x = out[,1], y = out[,2], lty = 2, lwd = 2, col = "red")
lines(x = out[,1], y = out[,3], lty = 2, lwd = 2, col = "red")

dat <- data.frame(0, seq(from = min(x), to = max(x), length.out = 1000))
colnames(dat) <- c("Tr","x")
pred <- predict.tree(temp.tree, newdata = dat)
lines(x = y1.true[,1], y = pred, lty = 3, lwd = 2, col = "blue")

dat <- data.frame(1, seq(from = min(x), to = max(x), length.out = 1000))
colnames(dat) <- c("Tr","x")
pred <- predict.tree(temp.tree, newdata = dat)
lines(x = y1.true[,1], y = pred, lty = 3, lwd = 2, col = "blue")


# add legend
lines(  x = seq(from = 2, to = 2.3, length.out = 10),
        y = rep(75,10), lty = 1, lwd = 2)
text(x = 2.3, y = 75, "True response curve", pos = 4, cex = .8)

lines(  x = seq(from = 2, to = 2.3, length.out = 10),
        y = rep(70,10), lty = 2, lwd = 2, col = "red")
text(x = 2.3, y = 70, "BART fit", pos = 4, cex = .8)

lines(  x = seq(from = 2, to = 2.3, length.out = 10),
        y = rep(65,10), lty = 3, lwd = 2, col = "blue")
text(x = 2.3, y = 65, "Single tree fit", pos = 4, cex = .8)

points(x = 2, y = 55, col = "black", pch = 21, bg = "white", cex = 1)
text(x = 2, y = 55, "Control obs", pos = 4, cex = .8)

points(x = 2, y = 60, col = "black", pch = 24, bg = "white", cex = .8)
text(x = 2, y = 60, "Treated obs", pos = 4, cex = .8)



