## To generate antithetic pairs of Poisson variables

## simple generator for Poisson distribution
## using inverse transform method

# function gives one sample from poisson distribution
randpoissonAV <- function (lambda) {
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


# function returns an antithetic pair from poisson distribution
randpoissonAV <- function (lambda) {
    x1 <- 0
    x2 <- 0
    sum1 <- exp(-lambda)
    sum2 <- exp(-lambda)
    prod1 <- exp(-lambda)
    prod2 <- exp(-lambda)
    U <- runif(1)
    while (U > sum1) {
        x1 <- x1 + 1
        prod1 <- prod1 * lambda / x1
        sum1 <- sum1 + prod1
    }
    
    while ((1-U) > sum2) {
        x2 <- x2 + 1
        prod2 <- prod2 * lambda / x2
        sum2 <- sum2 + prod2
    }
    return(c(x1, x2))
}



