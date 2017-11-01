source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')

Epsilon <- 1e-10
gradient_called <- 0
function_called <- 0

# # # #
# SR1 #
# # # #

#INITIALIZATION
x_k <- rep(2, 2)
times <- 0
H <- diag(length(x_k))
d <- data.frame(dx=c(0), fx=c(f(x_k)))
flag <- 0

for (i in 1:1500) {
  times <- i
  g_k <- gradient(x_k)
  d_k <- -g_k %*% H
  if (sum(d_k*g_k) > 0) {
    flag <- 1
    print ('The SR1 Method met a problem:')
    print ('in iteration ') 
    print (i) 
    print(', the approx Hesse matrix is not positive-defined')
    break
  }
  
  alpha_k <- preciseLinearSearch(d_k)
  temp <- x_k + alpha_k * d_k
  
  #improve H
  s_k <- alpha_k * d_k
  y_k <- gradient(temp)-g_k
  
  part1 <- t(s_k-y_k%*%H) %*% (s_k-y_k%*%H)
  part2 <- sum((s_k-y_k%*%H)*y_k)
  delta <- part1/part2[1]
  H <- H + delta
  
  d <- data.frame(dx=c(d$dx, sum((temp-x_k)^2)), fx=c(d$fx, f(temp)))
  
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # #')
print ('# SR1 #')
print ('# # # #')

if (flag == 0) {
  print('local minimum:')
  print (x_k)
  print ('--------------------------')
  print ('iteration times:')
  print (times)
  print ('--------------------------')
  print ('gradient function called times:')
  print (gradient_called)
  print ('--------------------------')
  print ('function called times:')
  print (function_called)
  print ('--------------------------')
  #print ('iteration process:')
  #print (d)
} else {
  print ('error occured')
}

#input <- readline('press enter...')
