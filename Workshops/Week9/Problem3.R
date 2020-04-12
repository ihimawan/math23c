# setwd("Workshops/Week9")
# cleanup
rm(list=ls())

"Use integrate to calculate the inner products needed to convert x3 into a vector
orthogonal to x. You have to use $value to extract the value of the integral."
x0 <- c(1,0,0,0,0,0)
x1 <- c(0,1,0,0,0,0)
x2 <- c(0,0,1,0,0,0)
x3 <- c(0,0,0,1,0,0)
x4 <- c(0,0,0,0,1,0)
x5 <- c(0,0,0,0,0,1)

"Using your inner products, dene the function P3(x) and plot it."