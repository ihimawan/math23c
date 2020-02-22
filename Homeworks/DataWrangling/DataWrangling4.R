# Problem 4 in Data Wrangling
#
# Find  on  the  Internet,  or  collect  on  your  own,  some  data  for  which  it  is a
# reasonable hypothesis that the number of counts in various bins mighthave arisen from a
# uniform distribution.  Save the result in a .csv file(whichmight have just one column.)
# Test the hypothesis of uniformity by usingthe  built-in  chi-square  test  in  R  and  by
# calculating  the  chi-square  value with  your  own  function  and  using pchisq().
# The  p  values  shoud  agree. The prototype is the analysis of athletes’ birth quarters
# in script 3D.

# obtaining dataset from https://www.kaggle.com/datasnaek/mbti-type
# The Myers Briggs Type Indicator (or MBTI for short) is a personality type system that
# divides everyone into 16 distinct personality types across 4 axis:
#
# Introversion (I) – Extroversion (E)
# Intuition (N) – Sensing (S)
# Thinking (T) – Feeling (F)
# Judging (J) – Perceiving (P)
#
# For these 16 personality types, I think it is reasonable to hypothesize
# that for all 16 personality types, they should be uniformly distributed.

# setwd("Homeworks/DataWrangling")
# cleanup
rm(list = ls())

MBTI <- read.csv("lib/mbti_1.csv")

observed <- table(MBTI$type)
# ENFJ ENFP ENTJ ENTP ESFJ ESFP ESTJ ESTP INFJ INFP INTJ INTP ISFJ ISFP ISTJ ISTP
#  190  675  231  685   42   48   39   89 1470 1832 1091 1304  166  271  205  337

# magnify for better results
barplot(observed,
        main = "MBTI personality distribution",
        xlab = "Personality trait",
        ylab = "Total")

# from the table itself, it doesn't look like they are equally distributed.
# but let's continue with the calculation ...

# how many different values are we using
n <- length(as.vector(observed)); # 16, which is the total number of the personality combinations
# how many times did we do this
M <- nrow(MBTI); # 8675. This many data!

expected <- rep(M/n, n)

barplot(expected,
        main = "MBTI personality distribution",
        xlab = "Personality trait",
        ylab = "Total")
chisq <- sum((observed-expected)^2 / expected); chisq # 9201.165

dfs <- n - 1
pValue <- pchisq(chisq, dfs, lower.tail = FALSE); pValue # 0
# this means that there is 0 probability that this result happens as a chance as a
# uniform distribution. that means we reject null hypothesis that our
# data came from a uniform distribution