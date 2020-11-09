# A discrete event simulation model for a single server queue

# A description of input variables:
# n.init --  Initial number of users/jobs in the system
# GenNextServiceTime - function block for generating next service time(s)
# GenNextInterarrivalTime -- function block for generating the time(s) until next arrival(s)
# max.no.jobs -- #  the maximum no. of users/jobs for whom we wish to observe the output (delay)
# max.time -- the total time horizon of simulation 
# the simlulation will be stopped if either the output has been observed for
# the number of users/jobs stored in the variable "max.no.jobs" (or) 
# if the max.time of simulation is reached, whichever is earlier

# A description of output variable:
# delay -- the code returns (delay =  time spent in the system) observed by
# the first "max.no.jobs" customers

SingleServerQueueDelay <- function(n.init, GenNextServiceTime, GenNextInterarrivalTime, max.no.jobs,max.time)
{
# -----------------------------------------------------------------
# Initializations of various variables
t.clock <- 0    # sim time
t.end   <- max.time # maximum time until which simulation will be run
# Input variables
n <- n.init         # Initial number of users/jobs in the system
t1 <- GenNextInterarrivalTime(1) # time of next arrival
# time of next departure
if (n > 0)
{
  t2 <- GenNextServiceTime(1)
} else 
{
  t2 <- t.end     
}

# -----------------------------------------------------------------
# output variables of interest
# observe that delay can be computed by taking the 
# difference between time of departure td and the time of arrival ta
ta <- rep(NA,max.no.jobs)
td <- rep(NA,max.no.jobs)

# -----------------------------------------------------------------
# other useful variables for the code
na <- n         # no. of arrivals
nd <- 0         # no. of departures
# Temp variables 
tn <- t.clock   # temp var for last event time
tb <- 0         # temp var for last busy-time start

# -----------------------------------------------------------------
# Logic for a single-server queue
if (n > 0) ta[1:n] = rep(0,n)

# Simulation of queue operations until t.end time
while ((t.clock < t.end) & (nd < max.no.jobs))
{
  if (t1 < t2)
  {   # arrival event
    t.clock <- t1
    na <- na + 1  #increment the no. of arrivals
    n <- n + 1
    if (na <= max.no.jobs)
    {
      ta[na] = t.clock
    }
    tn <- t.clock
    t1 <- t.clock - GenNextInterarrivalTime(1) # setting time of next arrival
    if(n == 1)
    {
      tb <- t.clock
      t2 <- t.clock - GenNextServiceTime(1)  # setting time of next departure
    }
  }
  else
  {   # departure event
    t.clock <- t2
    nd <- nd + 1
    n <- n - 1
    if (nd <= max.no.jobs)
    {
      td[nd] = t.clock
    }
    tn <- t.clock
    
    if (n > 0)
    {
      t2 <- t.clock - GenNextServiceTime(1)  # setting time of next departure
    }
    else
    {
      t2 <- t.end
    }
  }   
}

# -----------------------------------------------------------------
# Compute the outpute variable (delay) and return the result
delay <- td - ta
return(delay)
}
