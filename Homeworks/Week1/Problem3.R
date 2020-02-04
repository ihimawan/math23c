# cleanup
rm(list = ls())

attach(airquality)

# A) Using boxplot(), show how the values for each of these quantities
# depend on the month.
boxplot(Ozone ~ Month, na.rm = TRUE)
#Looks like ozone has a more variable value during month of July and August.

# B) With the aid of scatter plots, assess whether temperature does a good
# job of predicting solar radiation or ozone level.

plot(Temp, Solar.R, main = "Solar Radiation by Temperature",
     xlab = "Temperature ", ylab = "Solar Radiation")
# based on the scatter plot, Temperature and Solar Radiation are independent (which is unexpected
# because I intuitively thought the sunnier it is the hotter it is? But then there are some days when I go outside and
# it's hella sunny but like -1 Celcius)

plot(Temp, Ozone, main = "Ozone Level by Temperature",
     xlab = "Temperature ", ylab = "Ozone Level")
# Since a line model can be predicted, it seems like temperature can be a good predictor for Ozone Level

# C) Define logical vectors smoggy, windy, sunny, and hot, each of which
# is TRUE on days when the relevant quantity is higher than average.
# Find the dates on which all four are TRUE
averageOzone <- mean(Ozone, na.rm = TRUE); averageOzone #42.12931
averageSolar <- mean(Solar.R, na.rm = TRUE); averageSolar #185.9315
averageWind <- mean(Wind, na.rm = TRUE); averageWind #9.957516
averageTemp <- mean(Temp, na.rm = TRUE); averageTemp #77.88235

smoggy <- Ozone > averageOzone
windy <- Wind > averageWind
sunny <- Solar.R > averageSolar
hot <- Temp > averageTemp

allTrue <- smoggy & windy & sunny & hot;
answerC <- subset(airquality, allTrue, select = c("Month", "Day")); answerC
#     Month Day
# 29      5  29
# 40      6   9
# 81      7  20
# 100     8   8
# 104     8  12
# 112     8  20
# 134     9  11

# D) With the aid of the built-in chi-square test, find a pair of quantities
# that appears to be independent and one that does not.

# Test the hypothesis whether the Temperature is independent of Wind with .05 significance level.
# reference: http://www.r-tutor.com/elementary-statistics/goodness-fit/chi-squared-test-independence
chisq.test(Wind, Temp)
# 	Pearson's Chi-squared test
#
# data:  Wind and Temp
# X-squared = 1237.7, df = 1170, p-value = 0.0826
#
# Using 0.05 significance level, Since p-value = 0.0826 is greater than 0.05 significance level, we do not reject the
# null hypothesis that Temperature is independent of Wind
#
# This means Wind and Temperature may/may not have a correlation.

# Test the hypothesis whether the Temperature is independent of Ozone .05 significance level.
chisq.test(Temp, Ozone)
# 	Pearson's Chi-squared test
#
# data:  Temp and Ozone
# X-squared = 2699.6, df = 2508, p-value = 0.004036
#
# Using 0.05 significance level, Since p-value = 0.004036 is less than 0.05 significance level, we reject the
# null hypothesis that Temperature is independent of Ozone Level.
#
# This means Temperature and Ozone level has a correlation.

detach(airquality)