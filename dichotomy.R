dichotomy <- function (f) {
  #Epsilon is used as the breaking condition.
  Epsilon <- 1e-15
  
  #Initialization
  init <- 1
  res<-list()
  res$left <- init
  res$right <- init
  r_left <- init
  r_right <- init
  
  if (f(init) == 0) {
    return (init)
  }
  
  for (i in 0:1000) {
    s_left <- r_left
    s_right <- r_right
    r_left <- (1/2)*r_left
    r_right <- 2*r_right
    
    if (f(r_right) > 0 && f(s_right) < 0) {
      res$left <- s_right
      res$right <- r_right
      break
    }
    if (f(r_left) < 0 && f(s_left) > 0) {
      res$left <- r_left
      res$right <- s_left
      break
    }
  }
  #print ('in dichotomy')
  #print (res)
  for (i in 0:999) {
    mid <- (res$left+res$right)/2
    if (res$right - res$left <= Epsilon || f(mid) == 0) {
      #print (mid)
      return (mid)
    } else {
      if (f(mid)*f(res$left) > 0) {
        res$left <- mid
      } else if (f(mid)*f(res$left) < 0) {
        res$right <- mid
      }
    }
  }
}