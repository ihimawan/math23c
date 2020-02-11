# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

battle <- read.csv("lib/Battle.csv", stringsAsFactors = FALSE)

Unit1 <- "MEZU"
Unit2 <- "BRIG"

# obtain only a table with BRIG and MEZU to create appropriate samples
unitBattles <- subset(battle, Abbr == Unit1 | Abbr == Unit2)

# Find the average of kills - lost
unit1Battles <- subset(unitBattles, Abbr == Unit1)
unit2Battles <- subset(unitBattles, Abbr == Unit2)
scoreUnit1 <- unit1Battles$Kills - unit1Battles$Lost
scoreUnit2 <- unit2Battles$Kills - unit2Battles$Lost
observed <- mean(scoreUnit2) - mean(scoreUnit1) # 14.69173; it can be shown here that BRIG is winning

N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  # get sample of rows from the sub table which only contains BRIG and MEZU
  index <- sample(nrow(unitBattles), sum(unitBattles$Abbr == Unit1))
  selectedBattles <- unitBattles[index,]

  # calculate again the average of kills - lost
  unit1Battles <- subset(selectedBattles, selectedBattles$Abbr == Unit1)
  unit2Battles <- subset(selectedBattles, selectedBattles$Abbr == Unit2)
  scoreUnit1 <- unit1Battles$Kills - unit1Battles$Lost
  scoreUnit2 <- unit2Battles$Kills - unit2Battles$Lost
  diffs[i] = mean(scoreUnit2) - mean(scoreUnit1)
}
hist(diffs, breaks = "FD")
abline(v = observed, col = "red")
# From this historgram, it can be shown that "BRIG" has a more consistent win than "MEZU"