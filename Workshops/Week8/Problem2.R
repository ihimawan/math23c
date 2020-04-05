n <- 200
p <- 0.7
populationMean <- 140
populationSigma <- sqrt(42)

m <- 4
N <- 10^4

exp <- numeric(N)
stdSamplesB <- numeric(N)
stdSamplesC <- numeric(N)
stdSamplesT <- numeric(N)
for (i in 1:N){
  values <- rbinom(m, size=n, prob=p)
  sampleMean <- mean(values)
  sampleVar <- var(values)
  exp[i] <- (sampleMean - populationMean)/(populationSigma/sqrt(m))
  stdSamplesB[i] <- sum(((values-populationMean)/populationSigma)^2)
  stdSamplesC[i] <- sampleVar * (m-1) / populationSigma^2
  stdSamplesT[i] <- (sampleMean - populationMean)/(sqrt(sampleVar)/sqrt(m))
}

# A
hist(exp, probability = TRUE)
curve(dnorm(x, 0,1), add = TRUE, col="red")
# yes it is lol

# B
hist(stdSamplesB, probability = TRUE)
curve(dchisq(x, m), add = TRUE, col="red")
# kinda, so yes.

# C
hist(stdSamplesC, probability = TRUE)
curve(dchisq(x, m-1), add = TRUE, col="red")
# yayy it doesss

# D
hist(stdSamplesT, probability = TRUE, breaks="FD")
curve(dt(x, m-1), add = TRUE, col="red")
# the break saves us

