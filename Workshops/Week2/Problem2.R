# setwd("Workshops/Week2")
rm(list = ls()) # cleanup

bostonGames <- c(1,1,0)
stLouisGames <- c(1,0)

table <- expand.grid(Day1 = bostonGames,
                     Day2 = bostonGames,
                     Day3 = stLouisGames,
                     Day4 = stLouisGames,
                     Day5 = stLouisGames,
                     Day6 = bostonGames,
                     Day7 = bostonGames); head(table)
tableMod <- table
tableMod$totalWins <- rowSums(tableMod); head(table)
tableMod$totalWinsByDay6 <- rowSums(tableMod) - tableMod$totalWins - tableMod$Day7; head(tableMod)
length(which(tableMod$totalWinsByDay6 >= 4)) # 330

table(tableMod$totalWins)
probs <- table(tableMod$totalWins)/length(tableMod$totalWins)
barplot(probs)