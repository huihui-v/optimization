source('dichotomy.R')

preciseLinearSearch <- function (d_k) {
  target <- function (alpha_k) sum(gradient(x_k+alpha_k*d_k)*d_k)
  #print ('hahaha')
  #print (x_k+alpha_k*d_k)
  #curve(target)
  alpha_k <- dichotomy(target)
  #print ('in pre alpha_k')
  #print (alpha_k)
  return (alpha_k)
}