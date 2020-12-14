## parameter for inventory system
lambda <- 100 # mean demand size
c <- 2 # sales price
h <- 0.1 # inventory cost per item
K <- 10 # fixed ordering cost
k <- 1 # marginal ordering cost per item
p <- 0.95 # probability that order arrives
m <- 100 # number of days


## simple generator for Poisson distribution
## using inverse transform method
randpoisson <- function (lambda) {
    X <- 0
    cdf <- exp(-lambda)
    pmf <- exp(-lambda)
    U <- runif(1)
    while (U > cdf) {
        X <- X + 1
        pmf <- pmf * lambda / X
        cdf <- cdf + pmf
    }
    return (X)
}

## compute a realization of average profit
simulateOneRun <- function (s,S) {
    X <- S
    profit <- 0
    for (j in 1:m) {
        demand <- randpoisson(lambda)
        sales <- min(X,demand)
        Y <- X - sales
        if (Y < s && runif(1)<p) {
            profit <- profit - (K + k * (S-Y))
            X <- S }
        else {
            X <- Y }
        profit <- profit + c*sales - h*X
    }
    return (profit/m)
}

# differences between independent realizations of average profits
# using (80,200) policy and (80,198) policies
result.diff <- replicate(100,simulateOneRun(80,200)-simulateOneRun(80,198))

# confidence interval of differences between
# expected average profits over the next m months
ci.diff.rng <- t.test(result.diff,conf.level=0.95)$conf.int
message("CI using different random numbers:")
print(ci.diff.rng)
