# setwd("Homeworks/Week4")
# cleanup
rm(list = ls())

# A) creating the histogram for the delta
N <- 10000; delta <- numeric(N)
for (i in 1:N){
  # defines 4 subintervals [0,1]
  sortedSample <- sort(runif(3, 0, 1))
  deltas <- c(sortedSample[1], sortedSample[2] - sortedSample[1], sortedSample[3] - sortedSample[2], 1 - sortedSample[3])
  delta[i] <- max(deltas)
}

mean(delta) # 0.519298, this is the expectation
# getting this for 10^4 trials
hist(delta, breaks = "FD", probability = TRUE)

# B) write 3 functions for Riemman sums
# TODO NEED TO CHECK IF CORRECT

start <- 0
end <- 1

# the actual function from [0,1]
f <- function(x) x^2

# Left Riemann sum
RiemannL <- function(f, a, b, Part){
  Part <- c(a, Part, b)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i])
  }
  return(Rsum)
}
# Right Riemann sum
RiemannR <- function(f, a, b, Part){
  Part <- c(a, Part, b)
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i+1])
  }
  return(Rsum)
}

# Midpoint Riemann sum
RiemannMid <- function(f, a, b, Part){
  Part <- c(a, Part, b)
  n <- length(Part) - 1
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f(Part[i]+(b-a)/(2*n))
  }
  return(Rsum)
}

# the real integral x^2 [0,1] just for checking
integrate(f, start, end) # 0.3333333 with absolute error < 3.7e-15

Part <- sort(runif(3, start, end)); Part # 0.3018154 0.5410207 0.9829553
# finding left Riemann Sum [0,1]
RiemannL(f, start, end, Part) # 0.1676142 < 0.3333333 (real integral), as it should be
# finding right Riemann Sum [0,1]
RiemannR(f, start, end, Part) # 0.5415518 > 0.3333333 (real integral), as it should be
# finding midpoint Riemann Sum [0,1]
RiemannMid(f, start, end, Part) # 0.2652506 closer to 0.3333333 (real integral) compared to the left and right Riemann sum; as it should be.

# creating 10^4 trials for midpoint Riemann sums
N <- 10^4; sums <- numeric(N)
for (i in 1: N){
  Part <- sort(runif(3, start, end));
  sums[i] = RiemannMid(f, start, end, Part)
}

mean(sums) # 0.2580398
hist(sums, breaks="FD") # maximizing towards 0.2580398

# C) Simpson's Rule for correction
# TODO NEED TO CHECK IF CORRECT
RiemannSimpson <- function(f, a, b, n){
  Part <- c(a, Part, b)
  n <- length(Part) - 1
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*(4*f(Part[i]+(b-a)/(2*n))+f(Part[i])+f(Part[i+1]))/6
  }
  return(Rsum)
}

RiemannSimpson(f, start, end, Part) # 2.759472