step2 <- function (fi) {
  init <- 1
  res<-list()
  res$left <- init
  res$right <- init
  r_left <- init
  r_right <- init
  
  for (i in 0:10) {
    s_left <- r_left
    s_right <- r_right
    r_left <- (1/2)*r_left
    r_right <- 2*r_right
    if (fi(r_right) > 0 && fi(s_right) < 0) {
      #print ('case 1')
      res$left <- s_right
      res$right <- r_right
      break
    }
    if (fi(r_left) < 0 && fi(s_left) > 0) {
      #print ('case 2')
      res$left <- r_left
      res$right <- s_left
      break
    }
  }
  
  return (res)
}