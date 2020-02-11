# setwd("Workshops/Week2")
rm(list = ls()) # cleanup

battle <- read.csv("lib/Battle.csv", stringsAsFactors = FALSE); head(battle)

Unit1 <- "HOPH"
Unit2 <- "HOPL"

unitBattles <- subset(battle, Abbr == Unit1 | Abbr == Unit2);
unit1Battles <- subset(unitBattles, Abbr == Unit1);
unit2Battles <- subset(unitBattles, Abbr == Unit2)
scoreUnit1 <- unit1Battles$Kills - unit1Battles$Lost
scoreUnit2 <- unit2Battles$Kills - unit2Battles$Lost
mean(scoreUnit2) - mean(scoreUnit1)

N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  index <- sample(nrow(unitBattles), sum(unitBattles$Abbr == Unit1))
  selectedBattles <- unitBattles[index,]
  unit1Battles <- subset(selectedBattles, selectedBattles$Abbr == Unit1)
  unit2Battles <- subset(selectedBattles, selectedBattles$Abbr == Unit2)
  scoreUnit1 <- unit1Battles$Kills - unit1Battles$Lost
  scoreUnit2 <- unit2Battles$Kills - unit2Battles$Lost
  diffs[i] = mean(scoreUnit2) - mean(scoreUnit1)
}
diffs
hist(diffs)