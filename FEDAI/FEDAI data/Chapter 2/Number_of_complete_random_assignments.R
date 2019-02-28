# illustrate how the number of possible COMPLETE random assignments grows with N

maxN <- 20

N <- seq(from=4,to=maxN,by=2)

# first consider the case in which m = N/2
m <- N/2
perms <- factorial(N)/(factorial(m)*factorial(N-m))

# consider the alternative of simple random assignment
simple <- 2^N

# compare permutations
data.frame(N,m,perms,simple)

# next consider the case in which m = 4 but varying N
m <- 4
perms4 <- factorial(N)/(factorial(m)*factorial(N-m))
data.frame(N,m,perms4)