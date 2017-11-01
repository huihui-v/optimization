
preciseLinearSearch <- function (d_k) {
  
  target <- function (alpha_k) sum(gradient(x_k+alpha_k*d_k)*d_k)
  out <- 1.5
  for (i in c(1:1000)) {
    if (target(2^i) >= 0) {
      out <- 2^i
      break
    }
  }
  if (out == 1.5) {
    return (out)
  }

  r <- uniroot (target, c(0, out), tol=1e-10)
  alpha_k <- r$root

  return (alpha_k)
}