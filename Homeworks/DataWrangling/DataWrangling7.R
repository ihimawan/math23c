# Find on the Internet some data for which the histogram has a “bell curve” shape.
# Display a histogram of your data and overlay on it a graph of the normal density
# function for the same mean and variance.  Compare with the  normal  distribution
# by  using  deciles.   Then  repeat,  using  the  mean for samples of modest size.
# The prototype is the analysis of babies’ birthweights in script 5D.

# Data from: https://www.kaggle.com/unsdsn/world-happiness

# setwd("Homeworks/DataWrangling")
rm(list = ls())

WorldHappiness <- read.csv("lib/worldHappiness2019.csv"); head(WorldHappiness)
happyScore <- WorldHappiness$Score;
hist(happyScore, breaks="FD", probability = TRUE)
# world happiness score reasonably looks like a normal distribution.

nHappy <- length(happyScore) #the number of countries tested
#To find the best fitting normal distribution, compute the mean and variance of the data
mu <- mean(happyScore); mu # 5.407096
sigma <- sd(happyScore); sigma # 1.11312
curve(dnorm(x, mu, sigma), add = TRUE, col = "red")
# now it shows the histogram of the data with an overlay of graph of normal density function
# with the same mean and variance

# Now make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec   #11 bins
Exp <- rep(nHappy/10,10); Exp     #expected scores per bin

binScores <- numeric(10)
for (i in 1:10)
  binScores[i] <- sum((happyScore >= dec[i]) & (happyScore <= dec[i+1]) )

binScores
# [1] 16 18 20  8 17 11 14 22 10 20

chisq <- sum((binScores - Exp)^2/Exp); chisq # 12.84615
#We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 0.07594673
# Using 0.05 significance level, Since p-value 0.07594673 is greater than 0.05 significance level,
# we do not reject the null hypothesis that the normal distribution was a good model for the dataset.

# Now repeat, using average score for a sample of 25 happiness scores
N <- 2000; xbars <- numeric(N)
for (i in 1:N) xbars[i] <- mean(sample(happyScore,25))
hist(xbars, breaks = "FD", probability = TRUE)
# looks like a normal distribution here

#For the best fit, compute the mean and variance of the data
mu25 <- mean(xbars); mu25; mu # around 5.407096
sig25 <- sd(xbars); sig25; sigma # around 1.11312
curve(dnorm(x, mu25, sig25), add = TRUE, col = "red")

# Make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu25, sig25); dec   #11 bins
Exp <- rep(N/10,10); Exp     #expected means per bin

#Count how many means are in each bin
binmeans <- numeric(10)
for (i in 1:10)  binmeans[i] <- sum((xbars >= dec[i]) & (xbars <= dec[i+1]) ); binmeans

#Test for uniformity using chi square.
chisq <- sum((binmeans - Exp)^2/Exp); chisq #  12.46
#We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 0.08641063; # which is even larger than before
# Using 0.05 significance level, Since p-value 0.08641063 is larger than 0.05 significance level,
# we do not reject the null hypothesis that the normal distribution was a good model for the dataset.

#This time the calculated chi-square value could easily have arisen by chance
#This result illustrates the central-limit theorem:
#Start with any distribution of finite variance and construct the mean of many samples
#The sample means will have a distribution that is approximately normal