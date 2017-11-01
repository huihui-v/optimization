source('call.R')

f <- function(x) {
  call_function()
  m <- 31
  n <- length(x)
  if (n < 2 || n > m) {
    print ('error in x choosing')
  }
  res <- 0
  for (i in c(1:29)) {
    t_i <- i/29
    v1 <- c(1:(n-1))
    v2 <- x[2:n]
    v3 <- t_i^c(0:(n-2))
    part1 <- sum(v1*v2*v3)
    
    v4 <- t_i^c(0:(n-1))
    part2 <- sum(x*v4)^2
    
    res <- res + (part1 - part2 -1)^2
    #print (part1-part2-1)
  }
  
  res <- res + (x[1])^2
  res <- res + (x[2]-x[1]^2-1)^2
  return (res)
}