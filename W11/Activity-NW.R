# Project Review and Evaluation Technique network
# Example in class

SimulateOneRun <- function(a,b) {
  
  # Generate activity times
  U <- runif(13,0,1)
  Y <- a + (b-a)*U
  
  # compute activity times incurred in each path
  P1 <- Y[1] + Y[5] + Y[10]
  P2 <- Y[13] + Y[2] + Y[5] + Y[10]
  P3 <- Y[13] + Y[4] + Y[10]
  P4 <- Y[13] + Y[3] + Y[7] + Y[9] + Y[10]
  P5 <- Y[13] + Y[3] + Y[6] + Y[11] + Y[12]
  P6 <- Y[13] + Y[3] + Y[7] + Y[8] + Y[12]
  
  
  # return total activity time incurred
  T <- max(P1,P2,P3,P4,P5,P6)
  return(T)
}

# Output analysis (without CRN)
X1 <- replicate(100,SimulateOneRun(30,60))
X2 <- replicate(100,SimulateOneRun(32,58))

Z <- X1 - X2
ci.diff <- t.test(Z,conf.level=0.95)$conf.int
message("CI using different random numbers:")
print(ci.diff)

# Output analysis (with CRN)
set.seed(456)
X1 <- replicate(100,SimulateOneRun(20,70))

set.seed(456)
X2 <- replicate(100,SimulateOneRun(32,60))

Z.crn <- X1 - X2
ci.diff <- t.test(Z.crn,conf.level=0.95)$conf.int
message("CI using common random numbers:")
print(ci.diff)
