rm(list = ls(all = TRUE))

observ <- 60
iterat <- 5000

b0 <- 5
b1 <- 1
b2 <- 3

stsrs <- mtsrs <- stblock <- mtblock <- mtsrsx <- mtblockx <- matrix(NA,iterat,4)

for(i in 1:iterat)
{
x <- rep(0,observ)
x[(observ/2+1):observ] <- 1

treatseed <- rank(runif(observ),ties.method="random")

t_srs <- rep(0,observ)
t_srs[treatseed <= (observ/2)] <- 1

t_block <- rep(1,observ)
t_block[(observ/4+1):(observ*3/4)] <- 0

u <- rnorm(observ)

y1 <- b0 + b1*t_srs + b2*x + u
y2 <- b0 + b1*t_block + b2*x + u

st_srslm <- lm(y1~t_srs)
st_blocklm <- lm(y2~t_block)
mt_srslm <- lm(y1~t_srs+x)
mt_blocklm <- lm(y2~t_block+x)

stsrs[i,] <- summary(st_srslm)$coefficients["t_srs",]
stblock[i,] <- summary(st_blocklm)$coefficients["t_block",]
mtsrs[i,] <- summary(mt_srslm)$coefficients["t_srs",]
mtblock[i,] <- summary(mt_blocklm)$coefficients["t_block",]
mtsrsx[i,] <- summary(mt_srslm)$coefficients["x",]
mtblockx[i,] <- summary(mt_blocklm)$coefficients["x",]

}

## Mean Estimates
mean(unlist(stsrs[,1])) ## Simple, t_srs
mean(unlist(stblock[,1])) ## Simple, t_block
mean(unlist(mtsrs[,1])) ## Multiple, t_srs
mean(unlist(mtsrsx[,1])) ## Multiple, x
mean(unlist(mtblock[,1])) ## Multiple, t_block
mean(unlist(mtblockx[,1])) ## Multiple, x

## S.D. Estimates
sd(unlist(stsrs[,1])) ## Simple, t_srs
sd(unlist(stblock[,1])) ## Simple, t_block
sd(unlist(mtsrs[,1])) ## Multiple, t_srs
sd(unlist(mtsrsx[,1])) ## Multiple, x
sd(unlist(mtblock[,1])) ## Multiple, t_block
sd(unlist(mtblockx[,1])) ## Multiple, x

## Mean Standard Errors
mean(unlist(stsrs[,2])) ## Simple, t_srs
mean(unlist(stblock[,2])) ## Simple, t_block
mean(unlist(mtsrs[,2])) ## Multiple, t_srs
mean(unlist(mtsrsx[,2])) ## Multiple, x
mean(unlist(mtblock[,2])) ## Multiple, t_block
mean(unlist(mtblockx[,2])) ## Multiple, x