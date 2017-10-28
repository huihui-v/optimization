Hesse <- function (x) {
  v <- rep(0, 4)
  v[1] <- 6-2*x[2]
  v[2] <- -2*x[1]
  v[3] <- -2*x[1]
  v[4] <- 6
  m <- matrix(v, 2, 2)
  return (m)
}