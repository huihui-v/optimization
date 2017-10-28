source('f.R')

goldstein <- function(x_k, g_k, d_k) {
  rho <- 0.25

  alpha <- 1
  a <- 0
  b <- Inf
  f_k <- f(x_k)

  for (i in c(1:1000)) {
    print (f(x_k+alpha*d_k))
    print (f_k + rho*alpha*sum(g_k*d_k))
    if (f(x_k+alpha*d_k) > f_k + rho*alpha*sum(g_k*d_k)) {
      #print('case_1')
      b <- alpha
      alpha <- (a+b)/2
      next
    }
    if ((f(x_k+alpha*d_k) < f_k + (1-rho)*alpha*sum(g_k*d_k))) {
      #print('case_2')
      a <- alpha
      if (b == Inf) {
        alpha <- 2*alpha
      } else {
        alpha <- (a+b)/2
      }
      #print ('alpha3')
      #print (alpha)
      next
    }
    break
  }
  #print ('alpha:')
  #print(alpha)
  return (alpha)
}