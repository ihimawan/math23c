# setwd("Project")
# cleanup
rm(list=ls())

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
# [] A probability density graph overlaid on a histogram.
# [x] A contingency table.

# Required analysis
# []  A permutation test.
# []  A p-value or other statistic based on a distribution function.
# [x]  Analysis of a contingency table.
# []  Comparison of analysis by classical methods (chi-square, CLT) and simulation methods.
######### [To ALEX: Do we have to use ALL of these methods of analysis?]

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

# I want to see the age of those who attempted suicide
SuicideAge <- subset(ForeverAlone, attempt_suicide == "Yes", age)
hist(SuicideAge$age, main = "Age of those who attempted suicide",
    xlab ="Age of suicide",
    ylab = "number of people in this age that attempted suicide",
    col = "cyan")

# the histogram above looks a lot like a beta distribution...
######### [To ALEX: I'm having problems trying to find the shape parameter from the data :/]

# Contigency Table: Seeing if being depressed has any correlation to suicide attempt
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

########## [To ALEX: is this enough "analysis for the contigency table"? Not sure what else is there to analyze besides the CHISQ test?]
