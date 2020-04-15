# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"As a function of time, daily temperature is a fairly smooth function, and
using 100 Fourier coecients provided a good reconstruction for Boston.
Daily precipitation is not a smooth function of time. However, Califor-
nia has a rainy season and a dry season, and the annual cycle should be
revealed by Fourier analysis. Analyze the precipitation data in the le
LAWeather.csv using the same approach that was used for Boston temper-
ature in workshop problem 2. Introduce a variable ncoeff for the number
of Fourier coecients."

data <- read.csv("lib/LAWeather.csv")
precip <- data$precip; head(precip)
n <- length(precip)

# let's try to plot this
plot (1:n, precip, type="l")

# cosine & sine function from lecture
myCos <- function(m) cos((1:n)*m*2*pi/n)
mySin <- function(m) sin((1:n)*m*2*pi/n)

plot(1:n, precip, type="l")
points(1:n, 100*myCos(1), type="l", col="magenta")
points(1:n, 100*mySin(1), type="l", col="red")
points(1:n, 100*myCos(10), type="l", col="green")

# Evaluate the coefficients by Euler's formula 
coeffA <- function(m) sum(precip*myCos(m)/n)
coeffB <- function(m) sum(precip*mySin(m)/n)

"ncoef=10 discovers the annual cycle."

ncoeff <- 10

FourierA <- sapply(1:ncoeff,coeffA)
FourierB <- sapply(1:ncoeff,coeffB)
Fourier <- sqrt(FourierA^2+FourierB^2)

# first find the index that contains max value
largestIndex <- which.max(Fourier)

points(1:n, mean(precip) +
  FourierA[largestIndex]*myCos(largestIndex) +
  FourierB[largestIndex]*mySin(largestIndex), type = "l", col = "red",lwd = 2)

# reconstruct
recon <- mean(precip)
for (m in 1:ncoeff) {
  recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
}

plot(1:n, precip,type = "l")
points(1:n,recon, type = "l", col = "red",lwd = 2)
# looks like annual cycle (because there is pretty smooth and not much variation)

"ncoef=100 reveals that some years are rainier than others."

ncoeff <- 100

FourierA <- sapply(1:ncoeff,coeffA)
FourierB <- sapply(1:ncoeff,coeffB)
Fourier <- sqrt(FourierA^2+FourierB^2)

# first find the index that contains max value
largestIndex <- which.max(Fourier)

points(1:n, mean(precip) +
  FourierA[largestIndex]*myCos(largestIndex) +
  FourierB[largestIndex]*mySin(largestIndex), type = "l", col = "red",lwd = 2)

# reconstruct
recon <- mean(precip)
for (m in 1:ncoeff) {
  recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
}

plot(1:n, precip,type = "l")
points(1:n,recon, type = "l", col = "red",lwd = 2)
# some years are rainier than others due to some spikes

"ncoef=1000 does a very good reconstruction."

ncoeff <- 1000

FourierA <- sapply(1:ncoeff,coeffA)
FourierB <- sapply(1:ncoeff,coeffB)
Fourier <- sqrt(FourierA^2+FourierB^2)

# first find the index that contains max value
largestIndex <- which.max(Fourier)

points(1:n, mean(precip) +
  FourierA[largestIndex]*myCos(largestIndex) +
  FourierB[largestIndex]*mySin(largestIndex), type = "l", col = "red",lwd = 2)

# reconstruct
recon <- mean(precip)
for (m in 1:ncoeff) {
  recon <- recon + FourierA[m]*myCos(m)+FourierB[m]*mySin(m)
}

plot(1:n, precip,type = "l")
points(1:n,recon, type = "l", col = "red",lwd = 2)
# well matches the graph, good recon