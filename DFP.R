source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')

Epsilon <- 1e-10
gradient_called <- 0
function_called <- 0

# # # #
# DFP #
# # # #

#INITIALIZATION
#x_k <- c(0, 0, 2)
x_k <- rep(0, 15)
times <- 0
H <- diag(length(x_k))
d <- data.frame(dx=c(0), fx=c(f(x_k)))

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  d_k <- - g_k %*% H
  alpha_k <- preciseLinearSearch(d_k)
  temp <- x_k + alpha_k * d_k
  #improve H
  s_k <- alpha_k * d_k
  y_k <- gradient(temp)-g_k
  
  part1 <- (H %*% matrix(y_k) %*% y_k %*% H)/(y_k %*% H %*% matrix(y_k))[1]
  part2 <- (matrix(s_k) %*% s_k)/sum(y_k*s_k)
  delta <- -part1 + part2
  
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
print ('# DFP #')
print ('# # # #')

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

#input <- readline('press enter...')