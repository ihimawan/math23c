# Data Wrangling Problem 5
#
# Collect on your own (do NOT take data from the Internet), some data for which it is a reasonable hypothesis that the
# number of counts in variousbins ought to obey a Poisson distribution.  Examples:  emails per hour orper day, number of
# students arriving for lunch at Annenberg per minute.Save the resut in a .csv file(which might have just one column.)
# Display a histogram of your data side-by-side with a Poisson distribution that has the same mean.  and use a chi-square
# test to assess the match.  The prototypeis the analysis of Phillies’ home runs in script 3
#
# For this task, I took the most annoying book in the world, "How to Win Friends and Influence People" and counted the
# frequency of the letter 'e' (either lower case or upper case) per 1000 alphabetic characters (i.e. I would not include
# spaces, new lines, special characters, numbers, etc.).
#
# Oh no, I wouldn't count this by hand. I have the python code in the `helpers` directory if you'd like to take a look at
# my logic.

# setwd("Homeworks/DataWrangling")
rm(list = ls())

eFrequencies <- read.csv("lib/numberOfE.csv"); head(eFrequencies)
#       Range Frequency
# 1     0-999       129
# 2 1000-1999       112
# 3 2000-2999       109
# 4 3000-3999       148
# 5 4000-4999       132
# 6 5000-5999       123


# plot histogram of frequencies. Looks like a reasonable poisson distribution.
frequency <- eFrequencies$Frequency;
hist(frequency, breaks = "FD")
breaks <- hist(frequency, breaks = "FD")$breaks

# if this is a poisson distribution, find the mean
lambda <- mean(frequency); lambda # 125.9347

# seeing many tiny counts. going to try to clump them into buckets this way:
breaks <- hist(frequency, breaks = "FD", probability = TRUE)$breaks; breaks
#  [1]  80  85  90  95 100 105 110 115 120 125 130 135 140 145 150 155 160
observed <- hist(frequency, breaks = "FD", probability = TRUE)$count;
# [1]  1  0  1  2  5 15 27 56 53 66 47 35 11 13  4  1
#
# Looking better.. but still has small values. Going to clump first 4 buckets (x<=105)
# together as well as last 2 buckets (150 < x)
observed[5] <- sum(observed[1:5])
observed[15] <- sum(observed[15:16])
observed <- observed[-(1:4)]
observed <- observed[-12]; observed
#  [1]  9 15 27 56 53 66 47 35 11 13  5
# Looking more reasonable!

# Now just need to create the EXPTECTED BUCKETS
# The bins start from x<=105,105 < x <= 110, .... , 150 < x
N <- length(frequency); N # 338
expected <- N * c(sum(dpois(0:105, lambda)),
              sum(dpois(106:110, lambda)),
              sum(dpois(111:115, lambda)),
              sum(dpois(116:120, lambda)),
              sum(dpois(121:125, lambda)),
              sum(dpois(126:130, lambda)),
              sum(dpois(131:135, lambda)),
              sum(dpois(136:140, lambda)),
              sum(dpois(141:145, lambda)),
              sum(dpois(146:150, lambda)),
              ppois(150, lambda, lower.tail = FALSE))

# sanity check
sum(expected) # indeed equals to N = 338. Yay!

# put barplot side by side
barplot(rbind(observed, expected), beside = TRUE, col = c("red", "blue"))
# matches really well.

# use chisquared test
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

n <- length(observed)
# degrees of freedom is n-2 since we expected total equal to the actual total
# and computed lambda from data
chisq <- sum((observed-expected)^2/expected); chisq # 9.393988
pValue <- pchisq(chisq, df = n-2, lower.tail = FALSE); pValue # 0.4017285
# this means that there is 41.58% chance that this result happens as a
# poisson distribution. With this value, there is not enough evidence against the
# null hypothesis that our data did come from a poisson distribution.
#
# In fact the sample is consistent as a poisson distribution!