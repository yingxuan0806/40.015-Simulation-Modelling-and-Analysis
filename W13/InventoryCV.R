## parameter for inventory system
lambda <- 100 # mean demand size
c <- 2 # sales price
h <- 0.1 # inventory cost per item
K <- 10 # fixed ordering cost
k <- 1 # marginal ordering cost per item
p <- 0.95 # probability that order arrives
m <- 30 # number of days

# set.seed(01234)


## simple generator for Poisson distribution
## using inverse transform method
randpoisson <- function (lambda) {
    X <- 0
    sum <- exp(-lambda)
    prod <- exp(-lambda)
    U <- runif(1)
    while (U > sum) {
        X <- X + 1
        prod <- prod * lambda / X
        sum <- sum + prod
    }
    return (X)
}

# compute a realization of average profit
# return both the profit
# and the control variate (demand)
simulateOneRun <- function (s,S) {
    X <- S
    profit <- 0
    sum.demand <- 0
    for (j in 1:m) {
        demand <- randpoisson(lambda)
        sum.demand <- sum.demand + demand
        sales <- min(X,demand)
        Y <- X - sales
        if (Y < s && runif(1)<p) {
            profit <- profit - (K + k * (S-Y))
            X <- S }
        else {
            X <- Y }
        profit <- profit + c*sales - h*X
    }
    return (c(profit/m,sum.demand/m))
}

# average profits using (80,200) policy
n.rep <- 50
result <- replicate(n.rep,simulateOneRun(80,200))

X <- result[1,]
Y <- result[2,]

C.xy <- cov(X,Y)
var.y <- var(Y)
a <- C.xy/var.y

# usual estimator
message("The estimate from usual procedure is:")
print(mean(X))

# control variate based estimator
cv.est <- X - a*(Y-lambda)
message("The control variate based estimate is:")
print(mean(cv.est))


# variance of the usual estimate
# std.err.usual.est <- sqrt(var(X)/n.rep)
message("Variance of usual estimator is:")
print(var(X))

# variance of the control variate estimate
# std.err.cv.est <- sqrt(var(cv.est)/n.rep)
message("Variance of control variate based estimate is:")
print(var(cv.est))


# confidence interval from the usual estimate
ci.usual <- t.test(X,conf.level=0.95)$conf.int
message("CI using usual estimator:")
print(ci.usual)


# confidence interval from the control variate estimate
ci.cv <- t.test(cv.est,conf.level=0.95)$conf.int
message("CI using control variate based estimator:")
print(ci.cv)

