# setwd("Workshop/Week4")
# cleanup
rm(list = ls())

# A) Standard normal
f <- function(x) x^2*dnorm(x)
integrate(f, -Inf, Inf)
# 1 with absolute error < 1.2e-07

# B) rexp(4, rate=0.5)
# Chi square with 4 degrees of freedom

sumOfSquared <- function(vec) {
  sum <- 0
  for (i in 1 : length(vec)){
    sum <- sum + vec[i]^2
  }
  sum
}

# using 10^4 samples to generate chisq-esque distribution
N <- 10^4; vec <- numeric(N)
for (i in 1:N) {
  vec[i] <- sumOfSquared(rexp(4, rate=0.5))
}

# finding that this looks similar to chisq
hist(vec, breaks=20, probability = TRUE)

# overlaying with chisq function
curve(dchisq(x,4), add = TRUE, col = "red")