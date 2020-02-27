# setwd("Homeworks/Week4")
# cleanup
rm(list = ls())

# A)
# install.packages("resampledata")
library(resampledata)

# A) plot histogram for wait times
hist(Service$Times, probability = TRUE)
# looks like plot of density function for a gamma distribution

# B) overlay with dgamma r=2.65 and lambda=3.81
lambda <- 3.81; r <- 2.65

# overlaying with curve, looking like the sample is consistent with gamma distribution
curve(dgamma(x, shape = r, rate = lambda), add=TRUE)

# C) Finding expectation and variance

# finding expectation
EX <- integrate(function (x) x * dgamma(x, shape = r, rate = lambda), lower = 0, upper = Inf); EX
# 0.6955381 with absolute error < 1.8e-06

# finding variance
EXsq <- integrate(function (x) x^2* dgamma(x, shape = r, rate = lambda), lower = 0, upper = Inf);
Var <- EXsq$value - (EX$value)^2; Var
# 0.1825559

# checking to see if we can recreate this variance using a method of using samples
sampleVar <- var(rgamma(10000, shape = r, rate = lambda)); sampleVar
# value around 0.1869601 (This is comparing with population variance, but is very close to 0.1825559 above!)

# using built in...
sampleVar <- var(Service$Times); sampleVar
# value is 0.1819025 (which is similar to two above variances)

# D) Use chi-square to assess if gamma distribution

# using ten bins
bins <- qgamma(0.1 * (0:10),  shape = r, rate = lambda)
#[1] 0.00000000 0.04322688 0.08320508 0.12585916 0.17329786 0.22791820 0.29336562 0.37624613
# [9] 0.49119244 0.68457743        Inf
bincode <- cut(Service$Times, bins, labels = FALSE); head(bincode)
# 10 10  9 10  7  9 (looks reasonable)

Observed <- table(bincode); Observed
# 3  4  5  6  7  8  9 10
# 1  7  6  9 21 23 37 70
# none in the 1 or 2 buckets
Expected <- sum(Observed)/10 # 17.4
# Now we have observed and expected values and can compute the chi-square statistic.
Chi2 <- sum((Observed-Expected)^2/Expected); Chi2 # 1.977011

# How probable is this large a value, according the chi-square distribution?
# Since we have our expected total equal to actual equal
# and imposed the shape and a rate parameter, the df is 10-1-2 = 7
Pvalue <- pchisq(Chi2,7,lower.tail = FALSE); Pvalue # 0.9611018
# this means that there is 0.9611018 probability that this result happens as a
# gamma distribution. there is not enough evidence against the null hypothesis that our
# data did come from a gamma distribution
#
# In fact the sample is consistent as a gamma distribution