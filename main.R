source('f.R')
source('gradient.R')
source('preciseLinearSearch.R')
source('goldstein2.R')
source('Hesse.R')

Epsilon <- 1e-10


# # # # # # # # # # # #
#Simple Newton Method #
# # # # # # # # # # # #

#INITIALIZATION
x_k <- c(-2, 4)
times <- 0

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- Hesse(x_k)
  vec <- eigen(H)$values
  print (vec)
  err <- 0
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
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  print (temp)
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
} else {
  print ('error to stop')
}

input <- readline('press enter...')

# ==============================================================================

# # # # # # # # # # # # #
# Damping Newton Method #
# # # # # # # # # # # # #

#INITIALIZATION
x_k <- c(-2, 4)
times <- 0

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- Hesse(x_k)
  vec <- eigen(H)$values
  print (vec)
  err <- 0
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
  alpha_k <- preciseLinearSearch(d_k)
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  print (temp)
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # # # # # # # # # # #')
print ('# Damping Newton Method #')
print ('# # # # # # # # # # # # #')

if (err == 0) {
  print('local minimum:')
  print (x_k)
  print ('--------------------------')
  print ('iteration times:')
  print (times)
  print ('--------------------------')
} else {
  print ('error to stop')
}

input <- readline('press enter...')

# ==============================================================================

# # # # # # # # #
#Mixture Method #
# # # # # # # # #

#INITIALIZATION
x_k <- c(-2, 4)
times <- 0

for (i in 1:1000) {
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
  #print ('d_k')
  #print (d_k)
  alpha_k <- preciseLinearSearch(d_k)
  #print ('alpha_k')
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  #print (temp)
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

input <- readline('press enter...')

# ==============================================================================

# # # # # # # # # #
# Speedest Desent #
# # # # # # # # # #

#INITIALIZATION
x_k <- c(-5, 1)
times <- 0

for (i in 1:1000) {
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

input <- readline('press enter...')

# ==============================================================================

# # # # # # # # # # # # # # # # # # # # # #
#Speedest Desent with Goldstein Condition #
# # # # # # # # # # # # # # # # # # # # # #

#INITIALIZATION
x_k <- c(1.5, 1.5)
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

# ==============================================================================

# # # #
# SR1 #
# # # #

#INITIALIZATION
x_k <- c(-2, 4)
times <- 0

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- diag(length(x_k))
  
  d_k <- -g_k %*% H
  alpha_k <- preciseLinearSearch(d_k)
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  #improve H
  s_k <- alpha_k * d_k
  y_k <- gradient(temp)-g_k
  #print (matrix(s_k-y_k%*%H))
  #print (t(matrix(s_k-y_k%*%H)))
  #print (matrix(s_k-y_k%*%H) %*% t(matrix(s_k-y_k%*%H)))
  #print ((s_k-y_k%*%H)%*% matrix(y_k))
  delta <- (matrix(s_k-y_k%*%H) %*% t(matrix(s_k-y_k%*%H)))/((s_k-y_k%*%H) %*% matrix(y_k))[0]
  H <- H + delta
  #print (temp)
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


print('local minimum:')
print (x_k)
print ('--------------------------')
print ('iteration times:')
print (times)
print ('--------------------------')

input <- readline('press enter...')

# ==============================================================================

# # # #
# DFP #
# # # #

#INITIALIZATION
x_k <- c(-2, 4)
times <- 0

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- diag(length(x_k))
  
  d_k <- -g_k %*% H
  alpha_k <- preciseLinearSearch(d_k)
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  #improve H
  s_k <- alpha_k * d_k
  y_k <- gradient(temp)-g_k
  #print (matrix(s_k-y_k%*%H))
  #print (t(matrix(s_k-y_k%*%H)))
  #print (matrix(s_k-y_k%*%H) %*% t(matrix(s_k-y_k%*%H)))
  #print ((s_k-y_k%*%H)%*% matrix(y_k))
  delta <- ((matrix(s_k) %*% t(matrix(s_k)))/(s_k %*% matrix(y_k))[0]) - ((H%*%matrix(y_k))%*%t(H%*%matrix(y_k)))/(y_k %*% H %*% matrix(y_k))[0]
  H <- H + delta
  #print (temp)
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

input <- readline('press enter...')

# ==============================================================================

# # # #
#BFGS #
# # # #

#INITIALIZATION
x_k <- c(1.5, 10)
times <- 0

for (i in 1:1000) {
  times <- i
  g_k <- gradient(x_k)
  H <- diag(length(x_k))
  
  d_k <- -g_k %*% H
  alpha_k <- preciseLinearSearch(d_k)
  #print (alpha_k)
  temp <- x_k + alpha_k * d_k
  #improve H
  s_k <- alpha_k * d_k
  y_k <- gradient(temp)-g_k
  #print (matrix(s_k-y_k%*%H))
  #print (t(matrix(s_k-y_k%*%H)))
  #print (matrix(s_k-y_k%*%H) %*% t(matrix(s_k-y_k%*%H)))
  #print ((s_k-y_k%*%H)%*% matrix(y_k))
  delta <- (1+((y_k%*%H%*%matrix(y_k))/(y_k%*%matrix(s_k))[0]))*((matrix(s_k) %*% t(matrix(s_k)))/(s_k %*% matrix(y_k))[0]) - (matrix(s_k)%*%y_k%*%H + H%*%matrix(y_k)%*%s_k)/(y_k%*%matrix(s_k))[0]
  H <- H + delta
  #print (temp)
  if (sum((temp-x_k)^2) <= Epsilon) {
    x_k <- temp
    break
  } else {
    x_k <- temp
  }
}

print ('# # # #')
print ('#BFGS #')
print ('# # # #')


print('local minimum:')
print (x_k)
print ('--------------------------')
print ('iteration times:')
print (times)
print ('--------------------------')

input <- readline('press enter...')

# ==============================================================================

