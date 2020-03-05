# setwd("Homeworks/Week5")
# cleanup
rm(list = ls())

# A) Divide into (2^N)^3 dyadic cubes
f1 <- function (N) {
  delta <- 1/2^N # width of each dyadic cube
  left <- seq(from = 0, to = 1 - delta, by = delta)
  # get all left boundaries in 3 dimensions
  tbl <- expand.grid(left, left, left)
  # generate x, y, z points in every cube we have (2^N)^3 cubes
  # so, generate (2^N)^3 random numbers
  nums <- (2^N)^3
  X <- tbl$Var1 + runif(nums, 0, delta)
  Y <- tbl$Var2 + runif(nums, 0, delta)
  Z <- tbl$Var3 + runif(nums, 0, delta)
  score.mine <- mean(Z^2)
  score.opponent <- mean(X*Y)
  prob.win <- mean (Z^2 < X*Y)

  return (c(score.mine, score.opponent, prob.win))
}

# now do it 25 times. Strangely this is faster than the for loop
Repeat <- replicate(n=25, f1(6)); head(Repeat)

# finding sample mean and sample variance of my score
mean(Repeat[1,]) #0.3333302, close to 1/3 done in problem 5A
var(Repeat[1,]) # close to zero, which says that our approximation is pretty good

# finding sample mean and sample variance of opponent score
mean(Repeat[2,]) #0.2500006, close to 1/4 done in problem 5A
var(Repeat[2,]) # close to zero, which says that our approximation is pretty good

# sample mean and variance of winning probability
mean(Repeat[3,]) #0.4444365, close to 4/9 done in problem 5B
var(Repeat[3,]) # close to zero, which says that our approximation is pretty good

# B) using unif function
f2 <- function(N) {
  numCubes <- (2^N)^3
  #Since we want numCubes of x, y, z coordinates
  X <- runif(numCubes, 0, 1)
  Y <- runif(numCubes, 0, 1)
  Z <- runif(numCubes, 0, 1)

  mean.mine <- mean(Z^2)
  mean.opp <- mean(X*Y)
  prob.win <- mean(Z^2 < X*Y)

  return (c(mean.mine, mean.opp, prob.win))
}

# now do it 25 times. Strangely this is faster than the for loop
Repeat <- replicate(n=25, f2(6)); head(Repeat)

# finding sample mean and sample variance of my score
mean(Repeat[1,]) #0.3332762, close to 1/3 done in problem 5A
var(Repeat[1,]) # close to zero, which says that our approximation is pretty good

# finding sample mean and sample variance of opponent score
mean(Repeat[2,]) #0.2499726, close to 1/4 done in problem 5A
var(Repeat[2,]) # close to zero, which says that our approximation is pretty good

# sample mean and variance of winning probability
mean(Repeat[3,]) #0.4445734, close to 4/9 done in problem 5B
var(Repeat[3,]) # close to zero, which says that our approximation is pretty good

# noticing that the monte carlo method is more precise because the variation are much smaller.