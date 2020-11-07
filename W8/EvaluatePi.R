
###### Evaluating pi from the area of circle inscribed in the square [-1,1] x [-1,1]
EvaluatePi<-function(n)    #n= number of uniforms for each co-ordinate
{     
  x <- runif(n, -1, 1)    # generate n x-co-ordinates randomly
  y <- runif(n, -1, 1)   # generate n y-co-ordinates randomly
  I <- (x^2 + y^2 <= 1)

  plot.new()
  plot(x, y)
  curve(sqrt(1 - x^2), -1, 1, col = "blue", lwd = 3, add = T)
  curve(-sqrt(1 - x^2), -1 , 1, col = "blue", lwd = 3, add = T)
  
  avg_I = sum(I) / n
  pi_estimate <- 4 * avg_I;  # estimate of pi
  print(pi_estimate)
}


