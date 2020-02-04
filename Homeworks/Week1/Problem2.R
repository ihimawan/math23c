# setwd("Homeworks/Week1")
# cleanup
rm(list = ls())

# A) use it to determine how many participants in the survey were female Democrats who own guns.
gsl <- read.csv("lib/GSSLogical.csv")
attach(gsl)
answerA <- length(which(!Male & !Republican & GunOwner)); answerA #37
detach(gsl)

#B) Use it to find the shortest duration for a game in which the two teams together scored 10 or more runs.
rsx <- read.csv("lib/RedSox2013.csv")
attach(rsx)
# I don't know much about baseball, so I assume the `R` column includes the runs of two teams together
answerB <- which.min(Duration[which(R >= 10)]); answerB #1
detach(rsx)

#C) display separate box plots for the number of runs scored by the Sox in day games and in night games.
boxplot(R ~ DayNight, data = rsx)

#D) Build a 2x2 contingency table for WonLost and Away
attach(rsx)
table(WonLost, Away);
# WonLost FALSE TRUE
#       L    30   40
#       W    59   49
pWonLost <- mean(WonLost == 'L') #following the first value in above table to avoid confusion (for me)
pAway <- mean(!Away) #following the first value in above table to avoid confusion (for me)
expected <- nrow(rsx) * outer(c(pWonLost, 1 - pWonLost), c(pAway, 1 - pAway)); expected
#      [,1] [,2]
# [1,]   35   35
# [2,]   54   54

# Use anysterious calculation in R, confirmed by the built-in chi-square test to assess whether or not the Red Sox
# appear to have had a advantage for day or night games in 2013.

# Test the hypothesis whether the WonLost is independent of DayNight at .05 significance level.
# reference: http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence

chisq.test(WonLost, DayNight, correct = FALSE)
# 	Pearson's Chi-squared test
#
# data:  WonLost and DayNight
# X-squared = 0.90639, df = 1, p-value = 0.3411
#
# Using 0.05 significance level, Since p-value = 0.3411 is greater than 0.05 significance level, we do not reject the
# null hypothesis that Red Sox has an advantage during day or night games in 2013. Seems that with this, whether Red Sox
# wins/loses, it may not have anything to do with whether it was day or night.

detach(rsx)
