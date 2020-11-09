# The system considered is a single-server queueing system
# Interarrival times are exponentially distributed with mean avgIAtime
# Service times are exponentially distributed with mean avgServtime
# Customers are served on a First-Come-First-Serve basis

# Objective: To plot delay = time spent by user/job in the system

# Parameters specifying the queuing model 
avgIAtime <- 1.05    # expected interarrival duration
avgServtime <- 1    # expected service duration
n.init <- 10    #initial no. of users/jobs in the queue at time = 0

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
max.no.jobs <- 10000   #  the maximum no. of users/jobs for whom we wish to observe the output (delay)
max.time <- 1e6     # the maximum time until which simulation will be run
# the simlulation will be stopped if either the output has been observed for
# the number of users/jobs stored in the variable "max.no.jobs" (or) 
# if the max.time of simulation is reached, whichever is earlier

# -----------------------------------------------------------------
# Call the discrete-event simulation code for single-server queue to return
# the output delay
delay <- SingleServerQueueDelay(n.init, GenNextServiceTime, GenNextInterarrivalTime, max.no.jobs,max.time)

#-----------------------------------------------------------------------
#To visualize output
plot(seq(1,max.no.jobs), delay, type="s", ylim=c(0,200), col="blue",xlab="user/job number",ylab="Delay (time-spent in the system) for user/job",main="Single Server Queue Simulation")




