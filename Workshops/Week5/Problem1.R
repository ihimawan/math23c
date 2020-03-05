# setwd("Workshop/Week5")
# cleanup
rm(list=ls())

randNumbs <- runif(10^4, 0,1)
modified <- asin(2*randNumbs - 1)

hist(modified, breaks = 20, probability = TRUE)
curve(0.5*cos(x), add = TRUE, col = "red")
# matches!

randNumbs <- runif(10^4, 0,pi)
modified <- cos(randNumbs)

hist(modified, breaks = 40, probability = TRUE)
curve(1/(pi * sqrt(1-x^2)), add = TRUE, col = "red")

# yes the resulting density function is as what is stated!