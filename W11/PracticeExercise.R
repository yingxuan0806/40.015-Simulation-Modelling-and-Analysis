# To return n samples of a random variable which is uniformly distributed
# in the intervals [0,1] and [2,3]
n <- 1e5

# To define the inverse function for the given cdf
inv.f <- function(u) ifelse(u < 0.5, 2*u, 2*u+1)


# To use inverse transform method to obtain samples using the above defined inverse
sample <- GenInverseTransform(inv.f,n)

hist(sample, breaks = 25, freq = FALSE, main = "Plotting the sample histogram")


