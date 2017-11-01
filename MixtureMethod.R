source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')
source('Hesse.R')

Epsilon <- 1e-10
gradient_called <- 0
function_called <- 0
Hesse_called <- 0

# # # # # # # # #
#Mixture Method #
# # # # # # # # #

#INITIALIZATION
x_k <- rep(1, 15)
times <- 0
d <- data.frame(dx=c(0), fx=c(f(x_k)))

for (i in 1:10000) {
  times <- i
  g_k <- gradient(x_k)
  H <- Hesse(x_k)
  if (det(H) == 0) {
    d_k <- -g_k
  } else {
    d_k <- -g_k %*% solve(H)
    if (sum(g_k*d_k) > 1.5*sqrt(sum(g_k^2)*sum(d_k^2))) {
      d_k <- -d_k
    } else if (abs(sum(g_k*d_k)) <= 1.2*sqrt(sum(g_k^2)*sum(d_k^2))) {
      d_k <- -g_k
    }
  }
  alpha_k <- preciseLinearSearch(d_k)
  temp <- x_k + alpha_k * d_k
  
  d <- data.frame(dx=c(d$dx, sum((temp-x_k)^2)), fx=c(d$fx, f(temp)))
  
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # # # # # # #')
print ('#Mixture Method #')
print ('# # # # # # # # #')

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
print ('Hesse function called times:')
print (Hesse_called)
print ('--------------------------')
#print ('iteration process:')
#print (d)

#input <- readline('press enter...')