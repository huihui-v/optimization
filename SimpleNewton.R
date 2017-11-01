source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')
source('Hesse.R')

Epsilon <- 1e-10
gradient_called <- 0
function_called <- 0
Hesse_called <- 0

# # # # # # # # # # # #
#Simple Newton Method #
# # # # # # # # # # # #

#INITIALIZATION
x_k <- rep(0, 15)
times <- 0
err <- 0
d <- data.frame(dx=c(0), fx=c(f(x_k)))

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- Hesse(x_k)
  vec <- eigen(H)$values

  for (i in c(1:length(vec))) {
    if (vec[i] <= 0) {
      print ('When iterating, the algorithm met a matrix that is not p-d')
      print ('in iterator :')
      print (i)
      print ('matrix :')
      print (H)
      print ('vector :')
      print (vec)
      err <- 1
    }
  }
  if (err == 1) {
    break
  }
  d_k <- -g_k %*% solve(Hesse(x_k))
  alpha_k <- 1
  temp <- x_k + alpha_k * d_k

  d <- data.frame(dx=c(d$dx, sum((temp-x_k)^2)), fx=c(d$fx, f(temp)))
  
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # # # # # # # # # #')
print ('#Simple Newton Method #')
print ('# # # # # # # # # # # #')

if (err == 0) {
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
} else {
  print ('error occured')
}

#input <- readline('press enter...')