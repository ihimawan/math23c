# setwd("Homeworks/Week8")
# cleanup
rm(list=ls())

NCBirths <- read.csv("lib/NCBirths2004.csv")
populationMean <- mean(NCBirths$Weight) # 3448.26
populationSigma <- sd(NCBirths$Weight) # 487.736

n <- 6
N <- 10^4

stdSamplesA <- numeric(N)
stdSamplesB <- numeric(N)
stdSamplesC <- numeric(N)
stdSamplesT <- numeric(N)
for (i in 1:N){
  values <- sample(NCBirths$Weight, size=n)
  sampleMean <- mean(values)
  sampleVar <- var(values)
  stdSamplesA[i] <- (sampleMean - populationMean)/(populationSigma/sqrt(n))
  stdSamplesB[i] <- sum(((values-populationMean)/populationSigma)^2)
  stdSamplesC[i] <- sampleVar * (n-1) / populationSigma^2
  stdSamplesT[i] <- (sampleMean - populationMean)/(sqrt(sampleVar)/sqrt(n))
}

# A) Does have a distribution that is approximately standard normal?
hist(stdSamplesA, probability = TRUE, breaks = "FD")
curve(dnorm(x, 0,1), add = TRUE, col="red")
# yes it does!

# B) Does the sum of the squares of the standardized samples have a distribution that is approximately
# chi-square with n = 6 degrees of freedom?
hist(stdSamplesB, probability = TRUE, breaks = "FD")
curve(dchisq(x, n), add = TRUE, col="red")
# yes it does!

# C) If you multiply the sample variance by n - 1 and divide by sigma^2, does
# it have a distribution that is approximately chi-square with n - 1 = 5
# degrees of freedom?
hist(stdSamplesC, probability = TRUE, breaks="FD")
curve(dchisq(x, n-1), add = TRUE, col="red")
# It does!

# D) If you Studentize the sample mean, does the resulting random variable
# T have an approximate Student t distribution with n - 1 = 5
# degrees of freedom?
hist(stdSamplesT, probability = TRUE, breaks="FD")
curve(dt(x, n-1), add = TRUE, col="red")
# It absolutely does!