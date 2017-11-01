source('call.R')

gradient <- function (x) {
  call_gradient()
  m <- 31
  n <- length(x)
  if (n < 2 || n > m) {
    print ('error in x --- gradient')
  }
  g <- rep(0, n)
  t <- c(1:29)/29
  
  r <- rep(0, m)
  for (i in c(1:29)) {
    t_i <- i/29
    v1 <- c(1:(n-1))
    v2 <- x[2:n]
    v3 <- t_i^c(0:(n-2))
    part1 <- sum(v1*v2*v3)
    
    v4 <- t_i^c(0:(n-1))
    part2 <- sum(x*v4)^2
    
    r[i] <- part1 - part2 - 1
  }
  r[30] <- x[1]
  r[31] <- x[2]-x[1]^2 -1

  g_r <- rep(0, n)
  for (i in c(1:m)) {
    if (i == 30) {
      g <- rep(0, n)
      g[1] <- 1

      g_r <- g_r + g*r[30]
    } else if (i == 31) {
      g <- rep(0, n)
      g[1] <- -2*x[2]
      g[2] <- 1

      g_r <- g_r + g*r[31]
    } else {
      t_i <- i/29
      Sigma <- sum(x*t_i^(c(0:(n-1))))
      g <- rep(0, n)
      for (j in c(1:n)) {
        g[j] <- (j-1)*t_i^(j-2) - 2*Sigma*t_i^(j-1)
      }
      
      g_r <- g_r + g*r[i]
    }
  }
  
  return (2*g_r)
}