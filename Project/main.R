# setwd("Project")
# cleanup
rm(list=ls())

# use fitdistrplus
install.packages(fitdistrplus)
library(fitdistrplus)

# The Demographic /r/ForeverAlone Dataset: A survey taken by redditers in /r/ForeverAlone.
# Dataset source: https://www.kaggle.com/kingburrito666/the-demographic-rforeveralone-dataset

# Required dataset standards.
# [x] A dataframe.
# [x] At least two categorical or logical columns.
# [x] At least two numeric columns.
# [x] At least 20 rows, preferably more, but real-world data may be limited.
ForeverAlone <- read.csv("lib/foreveralone.csv");

# Required graphical displays (all graphs must be colored and nicely labeled)
# [x] A barplot.
# [x] A histogram.
# [x] A probability density graph overlaid on a histogram.
# [x] A contingency table.

# Required analysis
# []  A permutation test.
# [x]  A p-value or other statistic based on a distribution function.
# [x]  Analysis of a contingency table.
# []  Comparison of analysis by classical methods (chi-square, CLT) and simulation methods.
######### [To ALEK: Do we have to use ALL of these methods of analysis?]

# Required submission uploads
# [x]  A .csv file with the dataset
# []  A long, well-commented script that loads the dataset, explores it, and does all the analysis.
# []  A shorter .Rmd with compiled .pdf or .html file that presents highlights in ten minutes.
# []  A one-page handout that explains the dataset and summarizes the analysis.

# I want to see how many people in this subreddit has not/has attempted suicide depending on by their income -- does
# income level affect likelihood of suicide
count <- table(ForeverAlone$attempt_suicide, ForeverAlone$income)
# need to reorder the income range so that it is increasing
orderedCount <- count[,c(1,2,3,10,11,12,13,4,5,6,7,8,9)]
barplot(orderedCount, main = "Number of suicide attempts by income level",
        xlab = "Income Range",
        ylab = "Number of suicide attempts",
        legend.text = TRUE,
        las = 1,
        col = c("blue", "red"))
# to no surprise, it looks like those whose income is the lower end has a higher rate ot suicide attempts..
# this can also have come to light only because people who use the subreddit are mostly ones with lower income.

# I expect people who have least friends have a correlation to attempting who attempted suicide
SuicideFriends <- subset(ForeverAlone, attempt_suicide == "Yes", friends)
hist(SuicideFriends$friends, main = "Age of those who attempted suicide",
     xlab ="Number of friends",
     ylab = "Number of suicide attempts",
     col = "cyan")

# the histogram above looks a lot like a exponential distribution...
?fitdist
fit_e  <- fitdist(SuicideFriends$friends, "exp")
summary(fit_e)
#       estimate Std. Error
# rate 0.2652106 0.02876572
#
# let's try it!
rate <- 0.2652106
hist(SuicideFriends$friends, main = "Age of those who attempted suicide w/ probability",
     xlab ="Age of suicide",
     col = "cyan",
     probability = TRUE)
curve(dexp(x, rate = rate), add=TRUE, col="red")
# it looks pretty fitting! We can say that the more friends you have the less likely you are to attempt suicide

# let's try to test if this matches with a exponential distribution using distribution function
breaks <- hist(SuicideFriends$friends, breaks="FD")$breaks
#  [1]  0  2  4  6  8 10 12 14 16 18 20 22 24 26
observed <- hist(SuicideFriends$friends, breaks="FD")$count
# [1] 44 14 13  4  4  1  0  2  0  2  0  0  1
#
# Since there are some small numbers < 4, I will be combining bin 4 and bin 5, and all starting bin 6
# so the range for the bins are 0 < x < 2, 2 < x < 4, 4 < x < 6, 6 < x < 10, 10 < x
observed[4] <- sum(observed[4:5])
observed[5] <- sum(observed[6:13])
observed <- observed[-(6:13)]
# [1] 44 14 13  8  6
# Looking better!

numOfObservations <- sum(observed)
expected <- numOfObservations*c(pexp(2, rate),
                                pexp(4, rate)-pexp(2, rate),
                                pexp(6, rate)-pexp(4, rate),
                                pexp(10, rate)-pexp(6, rate),
                                1-pexp(10, rate))
# sanity check
sum(expected) # 85 ! yay!
Chi2 <-sum((observed-expected)^2/expected); Chi2 # 5.465886

# Since we have our expected total equal to actual equal
# and imposed the rate parameter, the df is 5-1-1 = 3
Pvalue <- pchisq(Chi2,3,lower.tail = FALSE); Pvalue # 0.1406933
# this means that there is 14% chance that this result happens as a
# exponential distribution. With this value, there is not enough evidence against the
# null hypothesis that our data did come from a exponential distribution.

# Permutation test: The mean number of friends for those who who attempted suicide is:
AttemptedFriendNumAvg <- mean(ForeverAlone$friends[which(ForeverAlone$attempt_suicide == "Yes")]) # 3.770588
NotAttemptedFriendNumAvg <- mean(ForeverAlone$friends[which(ForeverAlone$attempt_suicide == "No")]) # 8.883333
observed <- NotAttemptedFriendNumAvg - AttemptedFriendNumAvg; observed # 5.112745

# let's see if through permutation test, whether or not number of friends has relation to whether or not to attempt suicide
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Attempted <- sample(ForeverAlone$attempt_suicide)   # permuted suicide attempt column
  AttemptedFriendNumAvg <- mean(ForeverAlone$friends[which(Attempted == 'Yes')])
  NotAttemptedFriendNumAvg <- mean(ForeverAlone$friends[which(Attempted == 'No')])
  diffs[i] <- NotAttemptedFriendNumAvg - AttemptedFriendNumAvg
}

hist(diffs, breaks = "FD", probability = TRUE) ##### QUESTION: the permutation test looks crazy... why is it like this...?
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 0.00389961.. reject that they are independent

# Contigency Table: Seeing if feeling depressed has any correlation to suicide attempt
observed <- table(ForeverAlone$depressed, ForeverAlone$attempt_suicide); observed
  #      No Yes
  # No  147  10
  # Yes 237  75
# it's no surprise to me that those who are depressed are more likely to attempt suicide

# now let's try to compare with an expected contingency table
pDepressed <- mean(ForeverAlone$depressed == 'Yes')
pAttempted <- mean(ForeverAlone$attempt_suicide == 'Yes')
expected <- nrow(ForeverAlone) * outer(c(1 - pDepressed, pDepressed), c(1 - pAttempted, pAttempted)); expected
#          [,1]     [,2]
# [1,] 128.5458 28.45416
# [2,] 255.4542 56.54584
#
# looks pretty close from observed!

chisq <- sum((observed-expected)^2/expected); chisq # 21.97367
pValue <- 1 - pchisq(chisq, 1); pValue # 2.764167e-06
# With such a small p-value = 2.764e-06, we reject the null hypothesis that depression and suicide attempt are independent.
# meaning there may be a correlation between those who are depressed and those who have attempted suicide.

# using the built in chisq method...
chisq.test(ForeverAlone$depressed, ForeverAlone$attempt_suicide, correct=FALSE)
#
# 	Pearson's Chi-squared test
#
# data:  ForeverAlone$depressed and ForeverAlone$attempt_suicide
# X-squared = 21.974, df = 1, p-value = 2.764e-06
#
# (Same result)
# With such a small p-value = 2.764e-06, we reject the null hypothesis that depression and suicide attempt are independent.
# meaning there may be a correlation between those who are depressed and those who have attempted suicide.

########## [To ALEK: is this enough "analysis for the contigency table"? Not sure what else is there to analyze besides the CHISQ test?]
