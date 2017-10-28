source('step.R')
source('f.R')

goldstein2 <- function (x_k, g_k, d_k) {
  fi <- function (alpha) sum(gradient(x_k+alpha*d_k)*d_k)
  rho <- 0.25
  
  res <- step2(fi)
  #print (res)
  r_left <- res$left
  r_right <- res$right
  mid <- (r_left+r_right)/2
  result <- mid
  for (i in c(0:1000)) {
    if (f(x_k+mid*d_k) <= f(x_k)+rho*mid*sum(g_k*d_k)) {
      result <- mid
      break
    } else {
      r_right <- mid
      mid <- (r_left+r_right)/2
    }
  }
  return (result)
}