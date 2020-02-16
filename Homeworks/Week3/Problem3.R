# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

# ensure to install resample data first
library(resampledata)

# hypothesis is each number is equally likely to be drawn.
observed <- table(Lottery)
barplot(observed)
n <- max(Lottery) - min(Lottery) + 1
M <- nrow(Lottery)
expected <- rep(M/n, n)
chisq <- sum((observed-expected)^2 / expected); chisq
