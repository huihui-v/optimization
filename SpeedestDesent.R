source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')

Epsilon <- 1e-10


# # # # # # # # # #
# Speedest Desent #
# # # # # # # # # #

#INITIALIZATION
x_k <- rep(0, 5)
times <- 0

for (i in 1:10000) {
  times <- i
  d_k <- -gradient(x_k)
  g_k <- gradient(x_k)
  alpha_k <- preciseLinearSearch(d_k)
  temp <- x_k + alpha_k * d_k
  
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
  
}

print ('# # # # # # # # # #')
print ('# Speedest Desent #')
print ('# # # # # # # # # #')

print('local minimum:')
print (x_k)
print ('--------------------------')
print ('iteration times:')
print (times)
print ('--------------------------')

#input <- readline('press enter...')