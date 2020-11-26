# Project Review and Evaluation Technique network
# Example in class

SimulateOneRun <- function(a,b) {
  
  # Generate activity times
  U <- runif(13, 0, 1)
  Y <- a + (b - a) * U
  
  # alternative
  # Y <- runif(13, a, b)
  
  # compute activity times incurred in each path
  P1 <- Y[1] + Y[5] + Y[10]
  P2 <- Y[13] + Y[2] + Y[5] + Y[10]
  P3 <- Y[13] + Y[4] + Y[10]
  P4 <- Y[13] + Y[3] + Y[7] + Y[9] + Y[10]
  P5 <- Y[13] + Y[3] + Y[6] + Y[11] + Y[12]
  P6 <- Y[13] + Y[3] + Y[7] + Y[8] + Y[12]
  
  
  # return total activity time incurred
  Total_time <- max(P1,P2,P3,P4,P5,P6)
  return(Total_time)
}

# Output analysis (paired-t confidence)
# one replication
sys1 <- SimulateOneRun(20, 70)
sys2 <- SimulateOneRun(32, 60)

# multiple replication
set.seed(1)
sys1_rep <- replicate(100, SimulateOneRun(20, 70))

set.seed(1)
sys2_rep <- replicate(100, SimulateOneRun(32, 60))

# value is already paired here, no need to use paired argument in the t.test function
z <- sys1_rep - sys2_rep

t.test(z, conf.level = 0.95)
