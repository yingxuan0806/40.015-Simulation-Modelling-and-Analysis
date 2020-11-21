# To obtain n samples of RoI 
# example given in the lecture

GenRoI <- function(n) {
  # Initialize
  X <- rep(0,n) 
    
  for (i in 1:n) {
    # Generate a sample of Uniform[0,1]
    U <- runif(1, 0, 1)
  
    # Check the chain of IF conditions
    # and give a sample of x based on the value of U
    if ( U <= 0.07) 
      X[i] <- 9
    else if (0.07 < U && U <= 0.22)
      X[i] <- 10
    else if (0.22 < U && U <= 0.45)
      X[i] <- 11
    else if (0.45 < U && U <= 0.7)
      X[i] <- 12
    else if (0.7 < U && U <= 0.85)
      X[i] <- 13
    else if (0.85 < U && U <= 0.97)
      X[i] <- 14
    else
      X[i] <- 15
  }
  
  return(X)
}

run <- GenRoI(1000)
hist(run, breaks = c(5:20), freq = FALSE)
