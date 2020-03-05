# setwd("Workshops/Week5")
# cleanup
rm(list=ls())

BodyTemp <- read.csv("lib/BodyTemp.csv"); head(BodyTemp)

# A)
hist(BodyTemp$HeartRate, probability = TRUE)
numInBins <- hist(BodyTemp$HeartRate, probability = TRUE)$count; numInBins
# 4 14 25 32 31 19  5

# B)
avg <- mean(BodyTemp$HeartRate); avg # 73.76154
stdev <- sd(BodyTemp$HeartRate); stdev # 7.062077

# the agreement is pretty good
curve(dnorm(x, avg, stdev), add = TRUE, col = "red")

# C)
breaks <- hist(BodyTemp$HeartRate, probability = TRUE)$breaks; breaks
N <- length(BodyTemp$HeartRate); N #130

fx <- function (x) dnorm(x, avg, stdev)
pBin1 <- integrate(fx, 0, 60)$value
pBin2 <- integrate(fx, 60, 65)$value
pBin3 <- integrate(fx, 65, 70)$value
pBin4 <- integrate(fx, 70, 75)$value
pBin5 <- integrate(fx, 75, 80)$value
pBin6 <- integrate(fx, 80, 85)$value
pBin7 <- integrate(fx, 85, Inf)$value
Expected <- 130*c(pBin1,pBin2,pBin3,pBin4, pBin5, pBin6, pBin7); Expected
sum (Expected) # sanity check equals to 130!!!

#Now we have observed and expected values and can compute the chi-square statistic.
Chi2 <-sum((numInBins-Expected)^2/Expected); Chi2 # 2.421274

#How probable is this large a value, according the chi-square distribution?
#Since there are only 4 independent values, use 4 degrees of freedom
Pvalue<- pchisq(Chi2,4,lower.tail = FALSE); Pvalue
# The chance of this large a discrepancy arising by chance is about 0.6587862 (65%)
# We do not have enough evidence to reject that this is a normal distribution function

# D)
N <- 7305
samples <- numeric(N)
for(i in 1: N){
  sample <- sample(BodyTemp$HeartRate, 25, replace = FALSE)
  samples[i] <- mean(sample)
}

mu <- mean(samples) # 73.76139
sigma <- sd(samples)
hist(samples, probability = TRUE, breaks="FD") # peaking at 73.76139

fx <- function(x) dnorm(x, avg, stdev/5)
curve(fx, add = TRUE, col = "red")
# the agreement is very good