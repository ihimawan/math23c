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

frequency <- eFrequencies$Frequency;

barplot(table(frequency))

# if this is a poisson distribution, find the mean
lambda <- mean(frequency); lambda # 125.9347

observed <- table(frequency)
# seeing many tiny counts. going to try to clump them into buckets of:
# x <= 113, 116, 117, ..., 140 <= x

observed[15] <- sum(observed[0:15])
observed <- observed[-(0:14)]
observed[28] <- sum(observed[28:length(observed)])
observed <- observed[-(29: length(observed))]; observed
# 113 114 115 116 117 118 119 120 121 122 123 124 125 126 127 128 129 130 131 132 133 134 135 136 137 138 139 140
#  38   6   7  15  10   4  16  11  16  11   8   9   9  14  10  14  14  14  11  12   7  11   6  9   8  10   5  32
#
# Looking good!

# c(dpois(0:13, lambda), ppois(13, lambda, lower.tail = FALSE))

N <- nrow(eFrequencies)
expected <- c(sum(dpois(0:112, lambda)), dpois(113:140, lambda), ppois(140, lambda, lower.tail = FALSE))
sum(expected) # must sum up to 1

barplot(expected)
barplot(observed)

ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- sum((observed-expected)^2/expected); chisq
pValue <- pchisq(chisq, df = 14, lower.tail = FALSE); pValue
chisq.test(observed)