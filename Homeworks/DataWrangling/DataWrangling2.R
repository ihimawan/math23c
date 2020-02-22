# setwd("Homeworks/DataWrangling")
rm(list = ls()) # cleanup

# Find  on  the  Internet  some  data  about  the  sport  of  your  choice.
# Create a dataframe with some factor columns, some numeric columns, and some logical columns,
# and save it as a .csv file.  Calculate some statistics in thestyle  of  Script  1D,
# topic  2  (the  Red  Sox  data).   If  you  do  this  later  in the course, you can
# probably do some more sophisticated analysis, but the requirement is simply to create
# a useful .csv file and do a bit of analysis onit.

# I will be obtaining the dataset from here https://www.kaggle.com/dansbecker/nba-shot-logs/version/1#shot_logs.csv

shots <- read.csv("lib/shot_logs.csv", stringsAsFactors = FALSE)

# PART 1: Testing difference between number of dribbles and win/lose
WinDribbleAvg <- mean(shots$DRIBBLE[which(shots$W == "W")])
LoseDribbleAvg <- mean(shots$DRIBBLE[which(shots$W == "L")])
observed <- WinDribbleAvg - LoseDribbleAvg # Wins only have 0.09325093 more dribbles

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  WinLose <- sample(shots$W)   # permuted win/lose column
  WinDribbleAvg <- mean(shots$DRIBBLE[which(WinLose == "W")])
  LoseDribbleAvg <- mean(shots$DRIBBLE[which(WinLose == "L")])
  diffs[i] <- WinDribbleAvg - LoseDribbleAvg
}

mean(diffs) # -1.713211e-05
hist(diffs, breaks = 100)
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 9.999e-05
pvalue.2tailed <- pvalue*2; pvalue.2tailed # around 0.00019998
# the two tailed p-value says there is only 0.00019998 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is no difference in mean dribbles and toward the alternative hypothesis
# that there is a difference.

# PART 2: Finding relationship between W(Win)/L(Lost) or A(Away)/H(Home)
WonLost <- shots$W
Away <- shots$LOCATION
observed <- table(WonLost, Away); observed
#      A     H
#   L 35496 27978
#   W 28639 35956
pWonLost <- mean(WonLost == 'W')
pAway <- mean(Away == 'A')
expected <- nrow(shots) * outer(c(1 - pWonLost, pWonLost), c(pAway, 1 - pAway)); expected
#          [,1]     [,2]
# [1,] 31786.81 31687.19
# [2,] 32348.19 32246.81
chisq <- sum((observed-expected)^2/expected); chisq # 1718.971
pValue <- 1 - pchisq(chisq, 1); pValue # very small value
chisq.test(WonLost, Away, correct=FALSE)

#
# 	Pearson's Chi-squared test
#
# data:  WonLost and Away
# X-squared = 1719, df = 1, p-value < 2.2e-16
# Using 0.05 significance level, Since p-value < 2.2e-16 is less than 0.05 significance level, we reject the
# null hypothesis that the team has an advantage during away games in 2013.
