# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

battle <- read.csv("lib/Battle.csv", stringsAsFactors = FALSE)

Unit1 <- "MEZU"
Unit2 <- "BRIG"

# obtain only a table with BRIG and MEZU to create appropriate samples
unitBattles <- subset(battle, Abbr == Unit1 | Abbr == Unit2)

# Find the average of kills - lost
score <- unitBattles$Kills - unitBattles$Lost
idx <- which(unitBattles$Abbr == Unit2)
observed <- mean(score[idx]) - mean(score[-idx]) # 14.69173; it can be shown here that BRIG is winning

N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  # get sample of rows from the sub table which only contains BRIG and MEZU
  idx <- sample(nrow(unitBattles), sum(unitBattles$Abbr == Unit2))
  diffs[i] = mean(score[idx]) - mean(score[-idx])
}
mean(diffs) # -0.005671234
hist(diffs, breaks = "FD")
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 0.01569843
pvalue.2tailed <- pvalue * 2; pvalue.2tailed # around .03139686
# the two tailed p-value says there is only 0.03139686 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is no difference in mean kills minus lost between two Abbrs and toward the
# alternative hypothesis that there is a difference.