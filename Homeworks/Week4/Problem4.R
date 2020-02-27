# setwd("Homeworks/Week4")
# cleanup
rm(list = ls())

# A) checking formula using gamma built in function
gamma(3/4) * gamma(5/4) == pi * sqrt(2)/4 # TRUE

# B) using built in integrate function
gammaFx1 <- function(x) {
  x^(3/4-1) * exp(-x)
}

gammaFx2 <- function(x) {
  x^(5/4-1) * exp(-x)
}

x <- integrate(gammaFx1, 0, Inf); x
# 1.225417 with absolute error < 1.4e-05
y <- integrate(gammaFx2, 0, Inf); y
# 0.9064025 with absolute error < 3.3e-05

# check the values
x$value * y$value # 1.110721
pi * sqrt(2)/4 #  1.110721

# but when doing this, we see FALSE! Probably because of rounding error...
x$value * y$value == pi * sqrt(2)/4 #

# use round because of rounding error
round(x$value * y$value - pi * sqrt(2)/4) == 0 # TRUE

# C) using Monte Carlo
RiemannMonteCarlo <- function(f, a, b, N){
  ff <- function(x) f(x)*((x >=a)&(x <=b))   #make the function zero outside [a,b]
  aInt <- floor(a); bInt <- ceiling(b)
  Rsum <- 0
  for (i in 1:((bInt-aInt)*2^N)) {
    xEval <- runif(1,min=aInt,max=bInt)
    Rsum <- Rsum + ff(xEval)/(2^N)   #evaluate at a random point
  }
  return(Rsum)
}

# take b = 10^4; N <- 5
b<-10^4; N <- 5
# approximate gamma(3/4)
x_2 <- RiemannMonteCarlo(gammaFx1, 0, b, N); x_2 # close to x_2;
y_2 <- RiemannMonteCarlo(gammaFx2, 0, b, N); y_2 # close to gamma(5/4)
round(x_2 * y_2 - pi * sqrt(2)/4) == 0 # TRUE
