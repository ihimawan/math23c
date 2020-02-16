# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

P <- c((6/pi^2) * (1/(1:999)^2), .0006082)

# A) confirm sum is 1
sum(P); 1
table(P)

# Compute E(X)
sum(1:1000*P) # 5.158213
# Compute E(X^2)
EX2 <- sum((1:1000)^2 * P) # 1215.519; which is the E(X^2) for all
EX2.1000 <- 1000 ^ 2 * tail(P, n=1) # 608.2; which is the E(X^2) for X=1000

EX2.1000/EX2 # 0.5003623; which says the tiny X=1000 accounts for half of the E(X^2)

# B)
samplingFx <- function() {
#  ????????
}

N <- 10^4
smp <- numeric(N)
for (i in 1:N){
  smp[i] <- samplingFx()
}
hist(smp, breaks = 100)
mean(smp)

# C) display histogram of Delay column
Flights <- read.csv("lib/FlightDelays.csv")
delays <- table(Flights$Delay)
hist(delays, breaks="FD")

# D) find histogram and sample variance of first-second after playing game 10^4 times.