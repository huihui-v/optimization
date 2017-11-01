source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')
source('goldstein2.R')
source('Hesse.R')

Epsilon <- 1e-10


# # # # # # # # # # # # # # # # # # # # # #
#Speedest Desent with Goldstein Condition #
# # # # # # # # # # # # # # # # # # # # # #

#INITIALIZATION
x_k <- c(2, 4)
times <- 0

for (i in 1:10) {
  times <- i
  d_k <- -gradient(x_k)
  g_k <- gradient(x_k)
  alpha_k <- goldstein2(x_k, g_k, d_k)
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  #temp <- x_k + d_k
  
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # # # # # # # # # # # # # # # # # # # #')
print ('#Speedest Desent with Goldstein Condition #')
print ('# # # # # # # # # # # # # # # # # # # # # #')

print('local minimum:')
print (x_k)
print ('--------------------------')
print ('iteration times:')
print (times)
print ('--------------------------')

input <- readline('press enter...')