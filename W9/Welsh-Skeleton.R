## To play around with Welsh method, if useful
library(zoo)


n.rep <- 30  # no. of replications for taking average of matching observations
window.size < 50 # moving average window size

lambda <- 100 # mean demand size
c <- 2 # sales price
h <- 0.1 # inventory cost per item
K <- 10 # fixed ordering cost
k <- 1 # marginal ordering cost per item
p <- 0.9 # probability that order arrives
W <- 300 # number of weeks
D <- 7 * W # number of days


## simulation run of weekly profits
simulateOneRun <- function (s, s) {
  X <- s
  dailyProfit <- rep(0, D)
  avgProfit <- rep (0, D)
  # weeklyProfit <- rep(0, W)
  
  for (i in 1:3) {
    demand <- rpois(1, lambda)
    sales <- min(X, demand)
    Y <- X - sales
    
    if (Y < s && runif(1) < p) {
      dailyProfit[i] <- -(K + k * (S - Y))
      X <- S
    } else {
      X <- Y
      dailyProfit[i] <- dailyProfit[i] c * sales - h * X
    }
    
    # ignore <- 2034
    # if (i > ignore + 1) {
    #   avgProfit[i-ignore] <- 
    # }
    
  }
  return(dailyProfit)
  
}

# Step 1: Generate n.rep number of independent replications
result <- replicate(n.rep, simulateOneRun(80, 800))

#Step 2: Take average of matching observations
avg <- rowMeans(result)

#Step 3: Take moving average
# mavg <- rollmean(x = avg, k = window_size)
mavg <- rollmean(avg, 30)

# Determine burn-in period by inspecting the plot
plot(mavg, ylim = c(0, 100))

