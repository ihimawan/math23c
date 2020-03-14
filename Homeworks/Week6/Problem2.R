# setwd("Homeworks/Week6")
# cleanup
rm(list=ls())

# using x = 1 and x = 4 ounces of coffee
# using y = 2 and y = 6 ounces of milk
# above are uniform distribution

# amount of liquid must be between 4 and 8 ounces
# 4 <= x + y <= 8

# milk and coffee ratio must be between 1/3
# 1 <= y/x <= 3

N <- 3 * 10^4
milk <- numeric(N)
coffee <- numeric(N)
correctLatte <- numeric(N) # 1 if latte obeys restrictions above, 0 otherwise

for (i in 1:N) {
  coffee[i] <- runif(1, 1, 4)
  milk[i] <- runif(1, 2, 6)
  milkPlusCoffee <- milk[i] + coffee[i]
  coffeeDivMilk <- milk[i]/coffee[i]
  correctLatte[i] <- 4 <= milkPlusCoffee & milkPlusCoffee <= 8 & 1 <= coffeeDivMilk & coffeeDivMilk <= 3
}

mean(correctLatte) # about 0.5001333

# get vector that if latte survived, what is the milk amount
correctMilk <- milk * correctLatte
# get vector that if latte survived, what is the coffee amount
correctCoffee <- coffee * correctLatte

correctMilkAmount <- subset(correctMilk, correctMilk != 0)
correctCoffeeAmount <- subset(correctCoffee, correctCoffee != 0)

# sanity checks
length(correctMilkAmount)/N # 0.5001333
length(correctCoffeeAmount)/N # 0.5001333

Data <- data.frame(correctMilkAmount, correctCoffeeAmount)

nrow(Data)/N

mean(Data$correctMilkAmount) # 3.880386 ounces survived
mean(Data$correctCoffeeAmount) #  2.324571 ounces survived

# Total liquid for lattes that survive
totalLiquid <- Data$correctMilkAmount + Data$correctCoffeeAmount
mean(totalLiquid) # total is 6.204956 ounces of those that survived

ratioLiquid <- Data$correctMilkAmount / Data$correctCoffeeAmount
mean(ratioLiquid) # 1.775064 ounces ounces of those that survived

# histogram of total liquid
hist(totalLiquid, probability = TRUE)
# histogram of ratio
hist(ratioLiquid, probability = TRUE)