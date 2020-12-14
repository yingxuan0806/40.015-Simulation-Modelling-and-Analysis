## load library
library(rstream)

## parameter for inventory system
lambda <- 100 # mean demand size
c <- 2 # sales price
h <- 0.1 # inventory cost per item
K <- 10 # fixed ordering cost
k <- 1 # marginal ordering cost per item
p <- 0.95 # probability that order arrives
m <- 365 # number of days

## initialize streams of random numbers
gendemand <- new("rstream.mrg32k3a")
genorder <- new("rstream.mrg32k3a")

## simple generator for Poisson distribution that
## uses uniform random numbers from stream ’gendemand’
randpoisson <- function (lambda,gendemand) {
    X <- 0
    cdf <- exp(-lambda)
    pmf <- exp(-lambda)
    U <- rstream.sample(gendemand,1)
    while (U > cdf) {
        X <- X + 1
        pmf <- pmf * lambda / X
        cdf <- cdf + pmf
    }
    return (X)
}

## compute a realization of average profit
simulateOneRun <- function (s,S) {
    X <- S; profit <- 0
    for (j in 1:m) {
        demand <- randpoisson(lambda,gendemand)
        sales <- min(X,demand)
        Y <- X - sales
        if (Y < s && rstream.sample(genorder,1)<p) {
            profit <- profit - (K + k * (S-Y))
            X <- S }
        else {
            X <- Y }
        profit <- profit + c*sales - h*X
    }
    return (profit/m)
}

# to compare (80,200) policy and (90,198) policy
# using common random numbers
result.CRN <- replicate(100,
                           {
                             ## skip to beginning of next substream
                             rstream.nextsubstream(gendemand);
                             rstream.nextsubstream(genorder);
                             simulateOneRun(80,200)}
                           - {
                             ## reset to beginning of current substream
                             rstream.resetsubstream(gendemand);
                             rstream.resetsubstream(genorder);
                             simulateOneRun(80,198)})


# obtain 95% confidence interval for difference betwen profits
ci.crn <- t.test(result.CRN,conf.level=0.95)$conf.int
message("CI using common random numbers:")
print(ci.crn)
