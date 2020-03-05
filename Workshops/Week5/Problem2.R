# setwd("Workshop/Week5")
# cleanup
rm(list=ls())

# A)
f <- function(x) dcauchy(x, 5)
curve(f, from = 0, to = 10)

integrate(f, -Inf, Inf) # 1 with absolute error < 5.1e-07
# this looks correct because it's kind of 1

# B)
randNumbs <- rcauchy(25, 5)
mean <- mean(randNumbs); mean # 5.301689

N <- 10^3
means <- numeric(N)
for (i in 1:N){
  randNumbs <- rcauchy(25, 5)
  means[i] <- mean(randNumbs)
}

# limiting from -20 to 20 so that it looks nicer because it has a big outlier
hist(means, breaks = "FD", probability = TRUE)

# C
N <- 10^3
means <- numeric(N)
for (i in 1:N){
  randNumbs <- rcauchy(2500, 5)
  means[i] <- mean(randNumbs)
}

hist(means, breaks = "FD", probability = TRUE)

# D)
N <- 10^3
medians <- numeric(N)
for (i in 1:N){
  randNumbs <- rcauchy(25, 5)
  medians[i] <- median(randNumbs)
}

# looks better!
hist(medians, breaks = "FD", probability = TRUE)

N <- 10^3
medians <- numeric(N)
for (i in 1:N){
  randNumbs <- rcauchy(2500, 5)
  medians[i] <- median(randNumbs)
}

# looks even better bc the range is smaller
hist(medians, breaks = "FD", probability = TRUE)


