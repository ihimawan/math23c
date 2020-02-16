# setwd("Workshops/Week3")
# cleanup
rm(list = ls())

# prove E(X) is 5
lambda <- 5
sum(0:300 * dpois(0:300, lambda)); # 5

# how to get Variance from ppois
sum(ppois(0:25, lambda, lower.tail = FALSE))
# VARIANCE:
sum(0:300*2 * dpois(0:300, lambda)) - sum(0:300 * dpois(0:300, lambda)) #5