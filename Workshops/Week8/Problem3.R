# setwd("Workshops/Week8")
# cleanup
rm(list=ls())

BodyTemp <- read.csv("lib/BodyTemp.csv")
populationMean <- mean(BodyTemp$HeartRate)
populationSd <- sd(BodyTemp$HeartRate)

n <- 10

means <- numeric(N)
stdSamplesT <- numeric(N)
for (i in 1:N){
  values <- sample(BodyTemp$HeartRate, n)
  sampleMean <- mean(values)
  sampleSd <- sd(values)
  means[i] <- sampleMean
  stdSamplesT[i] <- (sampleMean - populationMean)/(sampleSd/sqrt(n))
}

hist(means, probability = TRUE)
curve(dnorm(x, populationMean, populationSd/sqrt(10)), add=TRUE, col="red")
# nice. very nice.

hist(stdSamplesT, probability = TRUE, breaks = "FD")
curve(dt(x, n-1), add=TRUE, col="red")
# pretty.

#Let's see how we do with a t confidence interval
missedL <- 0; missedU <- 0
plot(x =c(50,100), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
N <-1000
counter <- 0
for (i in 1:N) {
  x <- sample(BodyTemp$HeartRate, n)
  L <- mean(x) + qt(0.025, n-1) * sd(x)/sqrt(n) #usually less than the true mean
  U <- mean(x) + qt(0.975, n-1) * sd(x)/sqrt(n) #usually greater than the true mean
  if (populationMean <L) missedL <- missedL + 1 #+1 if we were wrong
  if (populationMean >U) missedU <- missedU + 1 #+1 if we were wrong
  if (L < populationMean && U > populationMean) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = populationMean, col = "red") #vertical line at true mean
(N-missedL-missedU)/N #what fraction of the time did interval include the true mean?; 0.962
missedL/N   #the confidence interval rarely lies to the right of the true mean; 0.022
missedU/N   #the confidence interval often lies to the right of the true mean; 0.016