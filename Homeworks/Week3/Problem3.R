# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

# ensure to install resample data first
# install.packages("resampledata")
library(resampledata)

# hypothesis is each number is equally likely to be drawn.
observed <- table(Lottery)
barplot(observed)

# from the table itself, it doesn't look equally distributed
# but let's continue with the calculation ...

# how many numbers are we using
n <- length(as.vector(observed))
# how many times did we do this
M <- nrow(Lottery)

expected <- rep(M/n, n)

barplot(expected, breaks="FD")
chisq <- sum((observed-expected)^2 / expected); chisq # 33.676

dfs <- n - 1
pValue <- pchisq(chisq, dfs, lower.tail = FALSE); pValue # 0.669616
# this means that there is 0.669616 probability that this result happens as a chance as a
# uniform distribution. there is not enough evidence against the null hypothesis that our
# data came from a uniform distribution
