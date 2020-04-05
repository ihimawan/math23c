# setwd("Workshop/Week7")
# cleanup
rm(list=ls())

die <- c(1,2,3,4,5,6)
die8 <- expand.grid(die,die,die,die,die,die,die,die)
columns <- table(die8[,1]+die8[,2]+die8[,3]+die8[,4]+die8[,5]+die8[,6]+die8[,7]+die8[,8])
columnsAsNum <- as.double(unname(columns))
sumOfCols <- sum(columnsAsNum)
probs <- columnsAsNum/sumOfCols

plot(8:48, probs, type = 'l')
plot(8:48, columnsAsNum/(6^8), type = "l")

mean(rowSums(die8))
mu <- mean(rowSums(die8)) # 28
sigma <- sd(rowSums(die8)) # 4.83046

curve(dnorm(x, mu, sigma), add = TRUE, col="red")
# matches!
