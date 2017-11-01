source('call.R')

Hesse <- function (x) {
  call_Hesse()
  m <- 31
  n <- length(x)
  H <- matrix(rep(0, n*n), n, n)
  
  for (i in c(1:m)) {
    if (i == 30) {
      M <- matrix(c(2,rep(0, n*n-1)), n, n)
      H <- H + M
    } else if (i == 31) {
      M <- matrix(rep(0, n*n), n, n)
      M[1] <- 6*x[1]^2-2*x[2]+2
      M[2] <- -2*x[1]
      M[n+1] <- -2*x[1]
      M[n+2] <- 1
      H <- H + M
    } else {
      M <- matrix(rep(0, n*n), n, n)
      t_i <- i/29
      g_r <- rep(0, n)
      Sigma <- sum(x*t_i^c(0:(n-1)))
      for (j in c(1:n)) {
        g_r[j] <- (j-1)*t_i^(j-2)+2*Sigma*t_i^(j-1)
      }
      part1 <- matrix(g_r)%*%g_r
      
      r_i <- sum(c(1:(n-1))*x[2:n]*t_i^c(0:(n-2))) - sum((x*t_i^(c(0:(n-1))))^2) - 1
      data <- rep(0, n*n)
      for (l1 in c(1:n)) {
        for (l2 in c(1:n)) {
          data[(l1-1)*n+l2] <- 2*t_i^(l1+l2-2)
        }
      }
      part2 <- r_i[1]*matrix(data, n, n)
     
      M <- 2*(part1+part2)
      H <- H + M
    }
  }
  return (H)
}