# setwd("Workshops/Week9")
# cleanup
rm(list=ls())

"Read the le Weather2010 and extract the max column as a vector."
Weather <- read.csv("lib/Weather2010.csv"); head(Weather)
Max <- Weather$max; head(Max)

"Write the functions myCos() and mySin() that calculate the Fourier basis func-
tions as vectors."

ndays <- length(Max)
myCos <- function(m) cos((1:ndays)*m*2*pi/ndays)
mySin <- function(m) sin((1:ndays)*m*2*pi/ndays)

plot(1:ndays, Max, type = "l")

"Write functions to calculate the Fourier coecients an and bn and use sapply to
compute 20 of each."
# Evaluate the coefficients by Euler's formula 
coeffA <- function(m) sum(Max*2*myCos(m)/ndays)
coeffB <- function(m) sum(Max*2*mySin(m)/ndays)

ncoeff <- 20
FourierA <- sapply(1:ncoeff,coeffA)
FourierB <- sapply(1:ncoeff,coeffB)
Fourier <- sqrt(FourierA^2+FourierB^2)

largestN <- which(max(Fourier)) # 22.81764
largestIndex <- 9
points(1:ndays, mean(Max) +
  FourierA[largestIndex]*myCos(largestIndex) +
  FourierB[largestIndex]*mySin(largestIndex), type = "l", col = "red",lwd = 2)

"Plot the sum of all components up through n = 20. This should show warmer
and colder winters but not daily
uctuations."
recon <- mean(Max)
for (m in 1:ncoeff) {
  recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
}

plot(1:ndays, Max,type = "l")
points(1:ndays,recon, type = "l", col = "red",lwd = 2)