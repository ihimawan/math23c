rm(list = ls())

attach(airquality)

#A
boxplot(Ozone~Month, na.rm=TRUE)
print("Looks like ozone has a more variable value during month of July and August.")

#B
plot(Temp, Solar.R, main="Solar Radiation by Temperature",
   xlab="Temperature ", ylab="Solar Radiation")
print("Since everything is just scattered without being able to predict a line model, it doesn't seem like Temperature can predict Solar Radiation")
plot(Temp, Ozone, main="Ozone Level by Temperature",
   xlab="Temperature ", ylab="Ozone Level")
print("Since a line model can be predicted, it seems like temperature can be a good predictor for Ozone Level")

#C
averageOzone <- mean(Ozone, na.rm = TRUE)
averageSolar <- mean(Solar.R, na.rm = TRUE)
averageWind <- mean(Wind, na.rm = TRUE)
averageTemp <- mean(Temp, na.rm = TRUE)

smoggy <- Ozone > averageOzone
windy <- Wind > averageWind
sunny <- Solar.R > averageSolar
hot <- Temp > averageTemp

allTrue <- smoggy & windy & sunny & hot;
answerC <- subset(airquality, allTrue, select = c("Month", "Day")); answerC

#D
# With the aid of the built-in chi-square test, find a pair of quantities
# that appears to be independent and one that does not.

detach(airquality)