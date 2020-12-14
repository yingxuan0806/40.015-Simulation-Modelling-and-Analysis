## parameter for inventory system
lambda <- 100 # mean demand size
c <- 2 # sales price
h <- 0.1 # inventory cost per item
K <- 10 # fixed ordering cost
k <- 1 # marginal ordering cost per item
p <- 0.95 # probability that order arrives
m <- 30 # number of days

set.seed(456)


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

## compute a a realization of average profit
simulateOneRun <- function (s,S) {
    X <- S
    profit <- 0
    total_monthly_demand <- 0
    for (j in 1:m) {
        demand <- randpoisson(lambda)
        total_monthly_demand <- total_monthly_demand + demand
        sales <- min(X,demand)
        Y <- X - sales
        if (Y < s && runif(1)<p) {
            profit <- profit - (K + k * (S-Y))
            X <- S }
        else {
            X <- Y }
        profit <- profit + c*sales - h*X
    }
    # return (profit/m)
    # return both X and Y in every simulation
    return(c(profit/m, total_monthly_demand/m))
}

# differences between independent realizations of average profits
# using (80,200) policy and (80,198) policies
n.rep <- 100
result <- replicate(n.rep,simulateOneRun(80,200))



# std.err of the estimate
std.err <- sqrt(var(result)/n.rep)
message("std error using independent simulation runs:")
print(std.err)

# confidence interval of differences between
# expected average profits over the next m months
ci.indep <- t.test(result,conf.level=0.95)$conf.int
message("CI using independent simulation runs:")
print(ci.indep)



# with control variate
X <- result[1,]
Y <- result[2,]

cov_xy <- cov(X, Y)
var_y <- var(Y)
a <- cov_xy / var_y

# usual estimator
mean(X)

# control variate estimator
cv_est <- X - a * (Y - lambda)
mean(cv_est)
