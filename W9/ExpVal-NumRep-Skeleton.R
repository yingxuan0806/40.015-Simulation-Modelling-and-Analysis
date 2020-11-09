
# To evaluate the expectation of S_N within an accuracy of $tol
# We want to be certain about this with at least 95% probability
# Here S_N = X_1 + X_2 + ... + X_N
# N is assumed to be from Poisson distribution with mean r
# X is assumed to be from Exponential distribution with mean mu

SimulateOneRun <- function(r, mu)
{
    # To generate 1 sample from Poisson distribution
    N <- rpois(1, r)
    
    # To generate N samples from Exponential distribution
    X <- rexp(N, 1 / mu)
    
    # To generate i-th sample of S_N
    S <- sum(X)
}

ExpectedPayout <- function(r, mu, tol) # tol: error tolerance
{
    # Initialization
    nrep.initial <- 100
    mean.S <-  0
    var.S <- 0
    i <- 1
    term.flag = FALSE

    # For each replication, generate S_N
    # Stop if #iterations > nrep.initial and 1.96*std-error < tolerance
    while(term.flag == FALSE)
    {
        # To obtain one sample of output variable whose expectation is to be computed
        S <- SimulateOneRun(r, mu)
        
        # To compute sample mean and sample variance
        mean.prev <- mean.S
        var.prev <- var.S
        mean.S <- mean.prev + (S - mean.prev)/(i)
        if (i > 1)
        {
            var.S <- (1 - 1 / (i - 1)) * var.prev + i * (mean.S - mean.prev) ^ 2
        }
        
        # Check for termination
        se <- sqrt(var.S / i)
        if ((i > nrep.initial) && ((1.96 * sqrt(var.S) / sqrt(i)) < tol)) {
            term.flag <- TRUE
        } else {
            i <- i + 1
        }
        
        
    }
    
    message("The estimate for expected total payout is:")
    print(mean.S)
    
    message("standard error of the estimate is:")
    print(se)
    
    message("The number of simulation runs after which this estimate has been output is:")
    print(i)
    
    return(c(mean.S,se,i))
}

ExpectedPayout(67, 210, 100)
