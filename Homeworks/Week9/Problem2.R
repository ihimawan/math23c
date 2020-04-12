# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The following vector shows the predicted temperature in Phoenix, AZ at
3-hour intervals, from 5 AM on April 8, 2020 to 5 AM on April 9, 2020."
temp <- c(59,58,71,80,77,67,62,58,55)

"Using Lagrange interpolation, fit a polynomial of degree 8 to these data,
and use it to estimate the temperature at 7 PM. Draw a graph of this
polynomial, with the specified data points shown by pch = 24"

