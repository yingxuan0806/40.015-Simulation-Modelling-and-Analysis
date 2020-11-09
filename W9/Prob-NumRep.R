
# To evaluate the probability of S_N being larger than x
# within an accuracy of $tol
# We want to be certain about this with at least 95% probability

# Here S_N = X_1 + X_2 + ... + X_N
# N is Poisson distributed with rate r
# X is assumed to be from Exponential distribution with mean mu

SimulateOneRun <- function(r, mu, x) # x is the error value provided
{
    # To generate 1 sample from Poisson distribution
    N <- rpois(1, r)
    
    # To generate N samples from Exponential distribution
    X <- rexp(N, 1 / mu)
    
    # To generate i-th sample of S_N
    S <- sum(X)
    
    # To generate i-th sample of the indicator for S_N > x
    if (S > x) {
        I = 1
    } else {
        I = 0
    }
}

ProbabilityBankruptcy <- function(r, mu, x, tol) # tol is the probability tolerance
{
    # Initialization
    nrep.initial <- 100
    mean.I <-  0
    var.I <- 0
    i <- 1
    term.flag = FALSE

    # For each replication, generate S_N
    # Stop if #iterations > nrep.initial and error < tolerance
    while (term.flag == FALSE)
    {
        # To obtain one sample of output variable whose expectation is to be computed
        I <- SimulateOneRun(r, mu, x)
        
        # To compute sample mean and sample variance
        mean.prev <- mean.I
        mean.I <- mean.prev + (I - mean.prev) / (i)
        var.I <- mean.I * (1 - mean.I)
        
        # Check for termination
        se <- sqrt(var.I / i)
        if (i > nrep.initial && 1.96 * se < tol)
        {
         term.flag <- TRUE
        }
        else
        {
            i <- i + 1
        }
    }
    
    message("The estimate for the probability of bankruptcy is:")
    print(mean.I)
    
    message("standard error of the estimate is:")
    print(se)
    
    message("The number of simulation runs after which this estimate has been output is:")
    print(i)
    
    return(c(mean.I, se, i))

}

ProbabilityBankruptcy(67, 210, 16000, 0.01)

