# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

# P from the homework
P <- c((6/pi^2) * (1/(1:999)^2), .0006082)

# A) confirm sum is 1
sum(P) == 1 #TRUE

# Compute E(X)
EX <- sum(1:1000*P); EX # 5.158213
# Compute E(X^2)
EX2 <- sum((1:1000)^2 * P) # 1215.519; which is the E(X^2) for all
EX2.1000 <- 1000 ^ 2 * tail(P, n=1) # 608.2; which is the E(X^2) for X=1000

EX2.1000/EX2 # 0.5003623; which says the tiny X=1000 accounts for half of the E(X^2)

# B) display 10000 samples from this distribution
smp <- sample(1:1000, 10^4, prob = P, replace = TRUE)
hist(smp, breaks = 100, probability = TRUE) # kind of looks like a Poisson distribution
mean(smp) # value like 4.886902, which is very close to the EX obtained above

# C) display histogram of Delay column
Flights <- read.csv("lib/FlightDelays.csv")
delays <- Flights$Delay
hist(delays, breaks="FD", probability = TRUE)

# D) find histogram and sample variance of first-second after playing game 10^4 times.
first <- sample(1:1000, size = 10^4, prob = P, replace = TRUE)
second <- sample(1:1000, size = 10^4, prob = P, replace = TRUE)
Y <- first-second

hist(Y, breaks = "FD", probability = TRUE)
# histogram is peaking at 0

var(Y) # 2217.088