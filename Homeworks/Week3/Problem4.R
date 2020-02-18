# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

RedSox <- read.csv("lib/RedSox2013.csv")
Nats <- read.csv("lib/Nats2019.csv")

# columns that contains a single data vector
Runs <- c(RedSox$R, RedSox$RA, Nats$R, Nats$RA)

# A) need to create barplot of number of runs by a team
barplot(table(Runs))

# B) Find lambda if this is a Poisson distribution
lambda <- mean(Runs); lambda  # 4.738235

# check variance is close to lambda
variance <- var(Runs); variance # 11.14493;
# this is not close to lambda, which says that this distribution is not poisson distribution

# C) create chi-squared test to see if runs per game is modeled by Poisson
# lump together large numbers
#
# I will clump together 14 and above since need value of 5 for chi squared test

N <- length(Runs)

# obtain expected list where 0:13 are individual values and >=14 are clumped
# this should total to N
expected <- N * c(dpois(0:13, lambda), ppois(13, lambda, lower.tail = FALSE))

# obtained observed values where 0:13 are individual values and >=14 are clumped
observed <- table(Runs)
observed[15] <- sum(observed[15:20])
observed <- observed[-(16:20)]

ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- ChiSq(observed, expected) #1101.949
# since we expected total equal to actual total and computed lambda from data,
# degrees of freedom is 15-2
pValue <- pchisq(chisq, 15-2, lower.tail = FALSE); # 2.168168e-227
# This is saying the probability of this happening is 2.168168e-227
# we reject the null hypothesis that the data is a poisson distribution.