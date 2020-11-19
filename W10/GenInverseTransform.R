# GenInverseTransform - to return n samples based on the inverse of the target CDF
 
GenInverseTransform <- function(inv.f, n) {
  
  # Non-vectorized version (only for explanation)  
  #
  #   sample <- rep(NA,n)
  #   
  #   for (i in 1:n) {
  #     u <- runif(1,0,1)              # Step 1
  #     sample[i] <- inv.f(u)          # Step 2
  #   }
  #   
  
  # Vectorized version
  U = runif(n,0,1);                     # Step 1
  sample <- inv.f(U)                    # Step 2
  
  return(sample)
}
