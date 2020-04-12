# setwd("Workshops/Week9")
# cleanup
rm(list=ls())

"Make a vector of the function evaluation points and a vector of the values of the
sine function at these points."

valuationPts <- c(0, pi/12, pi/8, pi/6, pi/4, pi/3)
valueOfSine <- sin(valuationPts) # 0.0000000 0.2588190 0.3826834 0.5000000 0.7071068 0.8660254

"Construct the 6 x 6 matrix P^{-1} and use solve to calculate P."
pInverse <- cbind(rep(1,6), valuationPts, valuationPts^2, valuationPts^3, valuationPts^4, valuationPts^5)
det(pInverse) # 7.533285e-07 not zero

p <- solve(pInverse)
round(p %*% pInverse) # all ones in the diagonal

"Write a function interp(x,val) to calculate the approximation to the sine func-
tion from Lagrange interpolation."

interp <- function(x, val) c(1,x,x^2,x^3,x^4,x^5) %*% p %*% val

"Check that your function gives reasonable results for pi/7 and pi/2"

interp(pi/7, valueOfSine) # 0.4338837
sin(pi/7) # 0.4338837, the same!

interp(pi/2, valueOfSine) # 1.000869
sin(pi/2) # 1, the same (approximately)!

"Define a vector of 500 values equally spaced between 0 and pi"

seq <- seq(0, pi, (pi - 0)/500)

"Use sapply() to compute a vector of values from Lagrange interpolation, and
plot the result. Overlay a graph of sin x."

fVal <- sapply(seq, interp, val = valueOfSine)
plot(seq, fVal, type = "l")
points(valuationPts, valueOfSine) # all in the line!
points(c(pi/7, pi/2), c(interp(pi/7, valueOfSine), interp(pi/2, valueOfSine)), col="green") # also in the line

# overlaying graph of sin x
curve(sin, add = TRUE, col="red") # matches well in the begining