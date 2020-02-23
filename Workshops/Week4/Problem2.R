# setwd("Workshop/Week4")
# cleanup
rm(list = ls())

expectedLength <- function (x, y) max(
  sqrt((x[1]-x[2])^2+ (y[1]-y[2])^2),
  sqrt((x[1]-x[3])^2+ (y[1]-y[3])^2),
  sqrt((x[2]-x[3])^2+ (y[2]-y[3])^2)
)

N <- 10^4; diff <- numeric(N)
for (i in 1:N){
  # Generate three random x coordinates and three random y coordinates.
  # (uniform distribution)
  xCoors <- runif(3, 0, 2)
  yCoors <- runif(3, 0, 1)
  diff[i] <- expectedLength(xCoors, yCoors)
}

# Now do this 10000 times in a for loop, plot a histogram, and calculate the sample mean.
hist(diff, breaks="FD")
# determine the expected length of thelongest piece of cable required
mean(diff) # 1.116809