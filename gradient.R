gradient <- function (x) {
  g <- rep(0, 2)
  g[1] <- 6*x[1]-2*x[1]*x[2]
  g[2] <- 6*x[2]-x[1]^2
  return (g)
}