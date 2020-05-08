# setwd("Project")
# cleanup
rm(list=ls())

# libraries to use
library("fitdistrplus")
library("stringr")
library("stats4")
library("e1071")
library("ggplot2")

"The Demographic /r/ForeverAlone Dataset: A survey taken by redditers in /r/ForeverAlone.
Dataset source: https://www.kaggle.com/kingburrito666/the-demographic-rforeveralone-dataset"

ForeverAlone <- read.csv("lib/foreveralone.csv")

#### INCOME & SUICIDE ####

# I want to see how many people in this subreddit has not/has attempted suicide depending on by their income
# In other words, does income level affect likelihood of suicide?
count <- table(ForeverAlone$attempt_suicide, ForeverAlone$income)
# need to reorder the income range so that it is increasing
orderedCount <- count[,c(1,2,3,8,10,11,12,13,4,5,6,7,9)]
barplot(orderedCount, main = "Number of suicide attempts by income level",
        xlab = "Income Range",
        ylab = "Number of suicide attempts",
        legend.text = TRUE,
        las = 1,
        col = c("blue", "red"))
# to no surprise, it looks like those whose income is the lower end has a higher rate ot suicide attempts.
# I wonder, is it statistically significant?
#
# It's a shame that the columns of salary is an enumeration instead of an actual value.
# I will create a function that given the text in $x to $y, it will find the average.
# I will add this as another extra column in the `ForeverAlone`.

# this function will convert the text "$x" or "$x to $y" and find the mean between x and y
findAverage <- function(row){
  incomeRange <- row[5]
  cleanRange <- str_remove_all(incomeRange, "[$,A-z]")
  nums <- as.numeric(strsplit(cleanRange, "\\s+")[[1]])
  if (length(nums) == 1){
    nums[1]
  } else {
    sum(nums)/length(nums)
  }
}

ForeverAlone$incomeAverage <- apply(ForeverAlone, 1, findAverage)
head(ForeverAlone$incomeAverage)
# [1] 34999.5  5000.5     0.0  5000.5 34999.5 62499.5
# nice, getting numerical columns now.

# Let's try to use logical regression to see the correlation
attemptedSuicide <- (as.numeric(ForeverAlone$attempt_suicide=="Yes")); head(attemptedSuicide)

income <- ForeverAlone$incomeAverage
plot(income, attemptedSuicide)
b <- cov(income, attemptedSuicide)/var(income) # obtain slope
# Find the intercept
a <- mean(attemptedSuicide) - b*mean(income);a
#We can add this regression line to the plot of the data
abline(a, b, col = "red") # a negative slope

MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*income)/(1+exp(alpha+beta*income)) )*attemptedSuicide +
          log(1/(1+exp(alpha+beta*income)))*(1-attemptedSuicide) )
}

#R has a function that will maximize this function of alpha and beta
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
# The curve shows a good model for the probability of being rich as function of schooling

# test for all the income values to see if it is a good model
uniqueIncomes <- sort(unique(income))
#  [1]      0.0   5000.5  14999.5  24999.5  34999.5  44999.5  62499.5  87499.5 112499.5 137499.5
# [11] 162499.5 187499.0 200000.0

findMean <- function (incomeToEvaluate) {
  index <- which(income == incomeToEvaluate); head(index)
  mean(attemptedSuicide[index])   #20%
}

res <- lapply(uniqueIncomes, findMean)
cleanRes <- numeric(length(res))
for (idx in 1: length(res)) {
  cleanRes[idx] = res[[idx]]
}

cleanRes
 # [1] 0.18125000 0.26000000 0.13793103 0.06818182 0.15384615 0.12500000 0.28571429 0.00000000
 # [9] 0.00000000 0.50000000 0.00000000 0.00000000 0.00000000

# Unfortunately from these numbers it doesnt look very promising for it to be statistically significant.. It fluctuates.
# But let's try to do a permutation test anyways.

# Permutation test: The mean number of salary for those who  attempted suicide is:
AttemptedIncomeAvg <- mean(ForeverAlone$incomeAverage[which(ForeverAlone$attempt_suicide == "Yes")]) # 18088.21
NotAttemptedIncomeAvg <- mean(ForeverAlone$incomeAverage[which(ForeverAlone$attempt_suicide == "No")]) # 20357.94
observed <- NotAttemptedIncomeAvg - AttemptedIncomeAvg; observed # 2269.724

# now let's use a permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Attempted <- sample(ForeverAlone$attempt_suicide)   # permuted suicide attempt column
  AttemptedIncomeAvg <- mean(ForeverAlone$incomeAverage[which(Attempted == 'Yes')])
  NotAttemptedIncomeAvg <- mean(ForeverAlone$incomeAverage[which(Attempted == 'No')])
  diffs[i] <- NotAttemptedIncomeAvg - AttemptedIncomeAvg
}

pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # 0.2918708
# If we say that statistically significant means they would need to have pvalue of <= 5%, this p-value is quite big (30%)
# even though it seems like the histogram shows some good correlation between income and suicide attempt.
# In fact, the p-value suggests that there is not enough evidence to reject the null hypothesis income and suicide is not independent.
# meaning income may/may not have correlation with suicide probability

#### FRIENDS & SUICIDE #####

# I expect people who have least friends have a correlation to attempting who attempted suicide
SuicideFriends <- subset(ForeverAlone, attempt_suicide == "Yes", friends)
hist(SuicideFriends$friends, main = "Number of suicide attempts vs number of friends",
     xlab ="Number of friends",
     ylab = "Number of suicide attempts",
     col = "cyan", breaks="FD")

# the histogram above looks a lot like a exponential distribution...
fit_e  <- fitdist(SuicideFriends$friends, "exp")
summary(fit_e)
#       estimate Std. Error
# rate 0.2652106 0.02876572
#
# let's try it!
rate <- 0.2652106
hist(SuicideFriends$friends, main = "Number of suicide attempts vs number of friends w/ probability",
     xlab ="Age of suicide",
     col = "cyan",
     probability = TRUE, breaks="FD")
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

pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # 0.00419958
# do reject that having friends and suicide attempt are independent. Meaning that there is strong correltion,
# of statistical significance, in-fact (if we're defining statistical significance as p<=5%),
# which is no surprise to me.

# Let's try to make the histogram of the `diff`
hist(diffs, breaks = "FD", probability = TRUE)
# Honestly, this looks quite crazy... is there something wrong done here?
# Possibly so many outliers? Let's try to plot the histogram of `ForeverAlone$friends`
hist(ForeverAlone$friends, breaks="FD") # there's mad amounts of outliers!

# In fact...
mean(ForeverAlone$friends) # 7.956716 is the mean
median(ForeverAlone$friends) # 3 seems extremely way off...
max(ForeverAlone$friends) # no wonder! 600 is the max! Maybe some people include internet friends as friends??
min(ForeverAlone$friends) # minimum is 0. The data ranges between 0 and 600. even though the median is 3.

# realized R does not have built in mode function...
findMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

findMode(ForeverAlone$friends) # mode is 0

# With the value of mean, median, and mode, we can tell that this graph has positive skew. Therefore we have to throw out the larger values.

# how skewed is this data?
skewness(ForeverAlone$friends)  # 14.17621 (positive skew) this is so bad! -- considering if we want
# according to https://brownmath.com/stat/shape.htm, a non-highly skewed data if the skewness is between -1 and 1.
# So let's look for the subset of the dataset if that skewness and see if there exist that subset where
# it is statistically significant

# trimFriendData gives you the subset of the dataset where a percentage of the data is cut out.
trimFriendData <- function (data, percentage) {
  newData <- data

  upper_quantile <- quantile(newData$friends, 1-percentage)
  lower_quantile <- quantile(newData$friends, percentage)

  newData$friends[newData$friends > upper_quantile] <- NA
  newData$friends[newData$friends < lower_quantile] <- NA

  subset(newData, !is.na(friends))
}

# With this information, let's redo it while trimming the dataset

# this function will create the subset of the `ForeverAlone` dataset percentage of data is trimmed
# This function will graph the difference, which would be a normal distribution (as long as the percentage is not close to zero)
# and will return the p-value given the percentage
findPValueWithTrimmedFriends <- function (percentage) {
  ForeverAlone.Trimmed <- trimFriendData(ForeverAlone, percentage)

  cat("This is the trimmed mean = ", mean(ForeverAlone.Trimmed$friends), " \n")
  cat("This is the skewness for your trimmed subset = ", skewness(ForeverAlone.Trimmed$friends), "\n")

  # Now let's redo the permutation test with this new table
  AttemptedFriendNumAvg <- mean(ForeverAlone.Trimmed$friends[which(ForeverAlone.Trimmed$attempt_suicide == "Yes")])
  NotAttemptedFriendNumAvg <- mean(ForeverAlone.Trimmed$friends[which(ForeverAlone.Trimmed$attempt_suicide == "No")])
  observed <- NotAttemptedFriendNumAvg - AttemptedFriendNumAvg; observed

  # let's see if through permutation test, whether or not number of friends has relation to whether or not to attempt suicide
  N <- 10000
  diffs <- numeric(N)
  for (i in 1:N){
    Attempted <- sample(ForeverAlone.Trimmed$attempt_suicide)   # permuted suicide attempt column
    AttemptedFriendNumAvg <- mean(ForeverAlone.Trimmed$friends[which(Attempted == 'Yes')])
    NotAttemptedFriendNumAvg <- mean(ForeverAlone.Trimmed$friends[which(Attempted == 'No')])
    diffs[i] <- NotAttemptedFriendNumAvg - AttemptedFriendNumAvg
  }

  hist(diffs, breaks = "FD", probability = TRUE) # BEAUTIFUL! It looks like a proper normal distribution :)
  abline(v = observed, col = "red")

  pvalue <- (sum(diffs >= observed)+1)/(N+1)
  cat("This is your p-value = ", pvalue, "\n")
}

# test out numbers, see which one is has low skewness and see if the p-value can still be statistically significant

findPValueWithTrimmedFriends(0.05)
# This is the trimmed mean =  4.464967
# This is the skewness for your trimmed subset =  1.475118
# This is your p-value =  0.02219778
#
# Small enough p-value.. but still to skewed.

findPValueWithTrimmedFriends(0.15)
# This is the trimmed mean =  3.130123
# This is the skewness for your trimmed subset =  0.8543304
# This is your p-value =  0.08429157
#
# Small enough skeweness but p-value too big

# after a couple of tries with many numbers... found that `0.11` is the sweetspot to keep
# the skewness low but find a statistically significant subset.
findPValueWithTrimmedFriends(0.11)
# This is the trimmed mean =  3.390168
# This is the skewness for your trimmed subset =  0.9407331
# This is your p-value =  0.04479552
#
# If we're using 0.05 significationscelevel (and if we define statistical significance as p-value <= 0.05),
# do not reject that having friends and suicide attempt are independent.
# they are actually statistically significant!

# Let's plot this as a logical regression graph to see if it can support this claim.

# this function will plot the logical regression graph and return a data frame of the number of friends
# and the percentage of those who attempted suicide
getLogicalRegressionValues <- function (percentage) {
  ForeverAlone.Trimmed <- trimFriendData(ForeverAlone, percentage)

  cat("This is the skewness for your trimmed subset = ", skewness(ForeverAlone.Trimmed$friends), "\n")

  # Let's try to see if using logistic regression can make it seem like it is statistical significant
  attemptedSuicide <- (as.numeric(ForeverAlone.Trimmed$attempt_suicide=="Yes")); head(attemptedSuicide)

  friends <- ForeverAlone.Trimmed$friends
  plot(friends, attemptedSuicide)
  b <- cov(friends, attemptedSuicide)/var(income) # obtain slope
  # Find the intercept
  a <- mean(attemptedSuicide) - b*mean(friends);a
  #We can add this regression line to the plot of the data
  abline(a, b, col = "red")

  MLL<- function(alpha, beta) {
    -sum( log( exp(alpha+beta*friends)/(1+exp(alpha+beta*friends)) )*attemptedSuicide +
            log(1/(1+exp(alpha+beta*friends)))*(1-attemptedSuicide) )
  }

  #R has a function that will maximize this function of alpha and beta
  results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
  results@coef
  curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
  # shows slight decline as number of friends grow

  # test for all the income values to see if it is a good model
  uniqueNumOfFriends <- sort(unique(friends))
  findMean <- function (numOfFriendsToEvaluate) {
    index <- which(friends == numOfFriendsToEvaluate); head(index)
    mean(attemptedSuicide[index])   #20%
  }

  res <- lapply(uniqueNumOfFriends, findMean)
  cleanRes <- numeric(length(res))
  for (idx in 1: length(res)) {
    cleanRes[idx] = res[[idx]]
  }
  cleanRes

  data.frame(number_of_friends = uniqueNumOfFriends, mean_attempted_suicide=cleanRes)
}

regressionWithThreshold <- getLogicalRegressionValues(0.11); head(regressionWithThreshold)
# It's strange because this logical regression value fluctuates a lot in this subset.
# But apparently it is statistically significant? Surprise!?

# Let's try to apply CLT to the number of friends to see if we can get the same normal distribution
ForeverAlone.NoFriendOutliers <- subset(ForeverAlone, friends <= 14) # use threshold as 14 form above
friendsNumber <- ForeverAlone.NoFriendOutliers$friends
hist(friendsNumber, breaks="FD", probability = TRUE) # looks nothing like a normaldistribution.

nFriends <- length(friendsNumber) # the number of tested
#To find the best fitting normal distribution, compute the mean and variance of the data
mu <- mean(friendsNumber); mu # 3.390168
sigma <- sd(friendsNumber); sigma # 3.393507
curve(dnorm(x, mu, sigma), add = TRUE, col = "red")
# now it shows the histogram of the data with an overlay of graph of normal density function
# with the same mean and variance

# Now make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec   #10 bins
Exp <- rep(nFriends/10,10); Exp     #expected friends per bin

binScores <- numeric(10)
for (i in 1:10)
  binScores[i] <- sum((friendsNumber >= dec[i]) & (friendsNumber <= dec[i+1]) )

binScores
# [1]   0 111  54  42  44  27  45  26  10  58

chisq <- sum((binScores - Exp)^2/Exp); chisq # 202.4484
#We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 3.477491e-40
# which makes sense because this is nothing like a normal distribution.

# Now repeat, using average score for a sample of 25 friends
N <- 2000; xbars <- numeric(N)
for (i in 1:N) xbars[i] <- mean(sample(friendsNumber,25))
hist(xbars, breaks = "FD", probability = TRUE)
# looks like a normal distribution here

#For the best fit, compute the mean and variance of the data
mu25 <- mean(xbars); mu25;  # around 3.416042
sig25 <- sd(xbars); sig25; # around 0.6499287
curve(dnorm(x, mu25, sig25), add = TRUE, col = "red") # looks good!

# Make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu25, sig25); dec   #10 bins
Exp <- rep(N/10,10); Exp     #expected means per bin

#Count how many means are in each bin
binmeans <- numeric(10)
for (i in 1:10)  binmeans[i] <- sum((xbars >= dec[i]) & (xbars <= dec[i+1]) ); binmeans
 # [1] 207 193 201 230 192 193 164 233 186 201

#Test for uniformity using chi square.
chisq <- sum((binmeans - Exp)^2/Exp); chisq #  8.68
# We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 0.2764543 # which is larger than before
# Using 0.05 significance level, Since p-value 0.2764543 is much larger than 0.05 significance level,
# we do not reject the null hypothesis that the normal distribution was a good model for the dataset.

#This time the calculated chi-square value could easily have arisen by chance
#This result illustrates the central-limit theorem:
#Start with any distribution of finite variance and construct the mean of many samples
#The sample means will have a distribution that is approximately normal.
#
# In this case the sample means is normal whether than the actual data is really not.

#### SUICIDE AND DEPRESSION ####

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

# Using the built in chisq method...
chisq.test(ForeverAlone$depressed, ForeverAlone$attempt_suicide, correct=FALSE)
#
# 	Pearson's Chi-squared test
#
# data:  ForeverAlone$depressed and ForeverAlone$attempt_suicide
# X-squared = 21.974, df = 1, p-value = 2.764e-06
#
# (Same result)
# With such a small p-value = 2.764e-06, we reject the null hypothesis that depression and suicide attempt are independent.

##### AGE AND SUICIDE #####

# Age of suicide : confidence interval
SuicideAge <- subset(ForeverAlone, attempt_suicide == "Yes", age)$age
populationMean <- mean(SuicideAge) # 24.24706
populationSd <- sd(SuicideAge) # 6.307414

n <- 10
N <- 10^4

means <- numeric(N)
stdSamplesT <- numeric(N)
for (i in 1:N){
  values <- sample(SuicideAge, n)
  sampleMean <- mean(values)
  sampleSd <- sd(values)
  means[i] <- sampleMean
  stdSamplesT[i] <- (sampleMean - populationMean)/(sampleSd/sqrt(n))
}

hist(means, probability = TRUE)
curve(dnorm(x, populationMean, populationSd/sqrt(10)), add=TRUE, col="red")
# looks nice

hist(stdSamplesT, probability = TRUE, breaks = "FD")
curve(dt(x, n-1), add=TRUE, col="red")
# looks nice.

#Let's see how we do with a t confidence interval
missedL <- 0; missedU <- 0
plot(x =c(15,40), y = c(1,100), type = "n", xlab = "", ylab = "") #blank plot
N <-1000
counter <- 0
for (i in 1:N) {
  x <- sample(SuicideAge, n)
  L <- mean(x) + qt(0.025, n-1) * sd(x)/sqrt(n) #usually less than the true mean
  U <- mean(x) + qt(0.975, n-1) * sd(x)/sqrt(n) #usually greater than the true mean
  if (populationMean <L) missedL <- missedL + 1 # +1 if we were wrong
  if (populationMean >U) missedU <- missedU + 1 # +1 if we were wrong
  if (L < populationMean && U > populationMean) counter <- counter + 1 #count +1 if we were correct
  if(i <= 100) segments(L, i, U, i)
}
abline (v = populationMean, col = "red") #vertical line at true mean
(N-missedL-missedU)/N #what fraction of the time did interval include the true mean?; 0.93
missedL/N   #the confidence interval rarely lies to the left of the true mean; 0.006
missedU/N   #the confidence interval more often lies to the right of the true mean; 0.064

# This is useful information because when we have campaigns regarding suicide,
# we know what age range to target.

# age of suicide & sexuality
ForeverAlone.SuicideSubset <- subset(ForeverAlone, attempt_suicide == "Yes")
ForeverAlone.SuicideSubset$isStraight <- ifelse(ForeverAlone.SuicideSubset$sexuallity == 'Straight', 1, 0)
ForeverAlone.SuicideSubset$isStraight <- factor(ForeverAlone.SuicideSubset$isStraight,levels=c(0,1),
   labels=c("Gay/Lesbian/Bisexual","Straight"))

# both are centered more in mid-20s
qplot(age, data=ForeverAlone.SuicideSubset, geom="density", fill=isStraight, alpha=I(.5),
   main="Distribution of Suicide rate by sexuality", xlab="Age",
   ylab="Density")

# both are centered more in mid-20s
qplot(isStraight, age, data=ForeverAlone.SuicideSubset, geom=c("boxplot", "jitter"),
   fill=isStraight, main="Mileage by Gear Number",
   xlab="", ylab="Miles per Gallon")

ForeverAlone$hasSuicide <- ifelse(ForeverAlone$attempt_suicide == "Yes", 1, 0)
ForeverAlone$isStraight <- ifelse(ForeverAlone$sexuallity == 'Straight', 1, 0)
ForeverAlone$isStraight <- factor(ForeverAlone$isStraight,levels=c(0,1),
   labels=c("Gay/Lesbian/Bisexual","Straight"))

qplot(age, hasSuicide, data=ForeverAlone, geom=c("point", "smooth"),
   method="lm", formula=y~x, color=isStraight,
   main="Regression of Suicide density on Age divided by sexuality",
   xlab="Age", ylab="Suicide density")

# seeing an overwhelming amount of people who are part of the LGBTQ community are more likely to attempt suicide.
# The likelihood of suicide doubles.

