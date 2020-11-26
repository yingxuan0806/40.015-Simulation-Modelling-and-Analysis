# To return n samples from an exponential distribution with mean mu
# Also plot the histogram of samples and compare with exponential density

n <- 1e5
mu = 1

# To define the inverse function for the given exponential cdf
inv.f <- function(u) -mu*log(1-u)

# To use inverse transform method to obtain samples using the above defined inverse
sample <- GenInverseTransform(inv.f,n)

hist(sample, breaks = 50, freq = FALSE, main = expression("Plotting the sample vs true density [for the example f(x) = "~mu*exp(-mu*x)~"]"))
curve(mu*exp(-mu*x),0,10,col="red",add=T)
