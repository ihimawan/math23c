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
  Rsum <- 0
  for (i in 1:(length(Part)-1)) {
    Rsum <- Rsum + (Part[i+1]-Part[i])*f((Part[i] + Part[i+1]) / 2)
  }
  return(Rsum)
}

# the real integral x^2 [0,1] just for checking
integrate(f, start, end) # 0.3333333 with absolute error < 3.7e-15

Part <- sort(runif(3, start, end)); Part # 0.3657477 0.4913086 0.8107473
# finding left Riemann Sum [0,1]
RiemannL(f, start, end, Part) # 0.2183018 < 0.3333333 (real integral), as it should be
# finding right Riemann Sum [0,1]
RiemannR(f, start, end, Part) # 0.4784583 > 0.3333333 (real integral), as it should be
# finding midpoint Riemann Sum [0,1]
RiemannMid(f, start, end, Part) # 0.32581 closer to 0.3333333 (real integral) compared to the left and right Riemann sum; as it should be.

# creating 10^4 trials for midpoint Riemann sums
N <- 10^4; sums <- numeric(N)
for (i in 1: N){
  Part <- sort(runif(3, start, end));
  sums[i] = RiemannMid(f, start, end, Part)
}

mean(sums) # 0.3166125, very close to 0.3333333 (real integral)
hist(sums, breaks="FD", probability = TRUE) # maximizing towards 0.33

# C) Simpson's Rule for correction
Part <- sort(runif(3, start, end));

RiemmanSimpson <- function(f, a, b, Part){
  left <- RiemannL(f, a, b, Part)
  right <- RiemannR(f, a, b, Part)
  mid <- RiemannMid(f, a, b, Part)
  return ((1/3)*((right+left)/2) + ((2/3) * mid))

}

RiemmanSimpson(f, start, end, Part) # 0.3333333, which is the same as the integral, it's a very good approximation for x^3 polynomials