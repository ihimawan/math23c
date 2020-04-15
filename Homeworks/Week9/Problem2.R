# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The following vector shows the predicted temperature in Phoenix, AZ at
3-hour intervals, from 5 AM on April 8, 2020 to 5 AM on April 9, 2020."

"Using Lagrange interpolation, fit a polynomial of degree 8 to these data,
and use it to estimate the temperature at 7 PM."

temp <- c(59,58,71,80,77,67,62,58,55)
interval <- 0:(length(temp)-1) # x-values for temp above

# create P^{-1} matrix to find P
Pinv <- cbind(1, interval, interval^2, interval^3,
              interval^4, interval^5, interval^6,
              interval^7, interval^8)
Pinv # this is a 9x9 matrix
P <- solve(Pinv)

# find interp function
interp <- function(x, val) {
  # this function relative to old basis
  c(1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8)%*%P%*%val
}

# estimate temp at 7PM
eval <- (19-5+3)/3; #17/3
interp(eval, temp) #  62.84198 as the estimated temperature for 7PM

"Draw a graph of this polynomial, with the specified data points shown by pch = 24"

# plot using sequence of 500 values evenly spaced between 0 to 8
xval <- seq(0,8, length.out = 500)
fval <- sapply(xval, interp, val=temp)

# looks pretty good.
plot (xval, fval, type="l", xlab = "hours after 5 in 3-hour increments", ylab="Temp")
points(interval, temp, pch=24)