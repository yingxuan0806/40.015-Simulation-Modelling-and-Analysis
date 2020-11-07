# To obtain samples of total payout in 1 year
# Here total payout S_N = X_1 + ... + X_N
# N is assumed to be from Poisson distribution with rate = r
# X_i are assumed to be iid exponentially distributed with mean = m

TotalPayout <- function(n, r, m)
{
  # Initialization
  N <- rep(0, n)  # For storing n samples of the no. of claims
  S <- rep(0, n) # For storing n samples of the total payout 
  
  # In every iteration, obtain one sample of the total payout S
  for(i in 1:n)
  {
    # To generate one sample of no. of claims in a year
    N[i] <- rpois(1, r) 
    
    # To obtain N claim samples
    X <- rexp(N[i], 1/m)
    
    # To obtain total payout
    S[i] <- sum(X)
  }
  
  return(S)
}