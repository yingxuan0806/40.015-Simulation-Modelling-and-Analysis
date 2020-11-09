# The system considered is a single-server queueing system
# Interarrival times are exponentially distributed with mean avgIAtime
# Service times are exponentially distributed with mean avgServtime
# Customers are served on a First-Come-First-Serve basis

# Objective: To obtain delay = time spent by user/job in the system for 
# a fixed number of independent simulation runs

# Parameters specifying the queuing model 
avgIAtime <- 1.3    # expected interarrival duration
avgServtime <- 1    # expected service duration
n.init <- 0    #initial no. of customers in the queue at time = 0

# Code for generating a sample of next service time
GenNextServiceTime <- function(k)
{
  NextServTime <- avgServtime*log(1-runif(k,0,1))
  return(NextServTime)
}

# Code for generating a sample of next interarrival time
GenNextInterarrivalTime <- function(k)
{
  NextInterarrivalTime <- avgIAtime*log(1-runif(k,0,1))
  return(NextInterarrivalTime)
}

# -----------------------------------------------------------------
# Simulation-specific variables 
n.rep <- 25
max.no.jobs <- 10000   #  the maximum no. of customers for whom we wish to observe the output (delay)
max.time <- 1e5     # the maximum time until which simulation will be run
# the simlulation will be stopped if either the output has been observed for
# the number of customers stored in the variable "max.no.jobs" (or) 
# if the max.time of simulation is reached, whichever is earlier

# -----------------------------------------------------------------
# Call the discrete-event simulation code for single-server queue to return
# the output delay
delay <- matrix(NA,nrow=n.rep,ncol=max.no.jobs,byrow=TRUE)
for (iter in 1:n.rep)
{
  delay[iter, ] <- SingleServerQueueDelay(n.init, GenNextServiceTime, GenNextInterarrivalTime, max.no.jobs,max.time)
}

#-----------------------------------------------------------------------
#To write output to csv file
write.csv(delay, file = "~/Desktop/delay.csv",row.names=FALSE)
#write.xlsx(delay, file = "Delay2019.xlsx",, sheetName = "Sheet1", 
#           col.names = TRUE, row.names = TRUE, append = FALSE)


