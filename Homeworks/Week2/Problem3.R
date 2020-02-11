# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

NC <- read.csv("lib/NCBirths2004_.csv"); head(NC)

# Testing difference between mean weight is significant
MaleAvg <- mean(NC$Weight[which(NC$Gender == "Male")])
FemaleAvg <- mean(NC$Weight[which(NC$Gender == "Female")])
observed <- MaleAvg - FemaleAvg; observed # Males are 103.26 ounces heavier

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(NC$Gender)   #permuted gender column
  MaleAvg <- mean(NC$Weight[which(Gender=="Male")])
  FemaleAvg <- mean(NC$Weight[which(Gender=="Female")])
  diffs[i] <- MaleAvg - FemaleAvg    #as likely to be negative or positive
}

mean(diffs)
hist(diffs, breaks = "FD")
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # small number

# Testing significance of tobacco with birth weight
NoTobaccoAvg <- mean(NC$Weight[which(NC$Tobacco == "No")])
TobaccoAvg <- mean(NC$Weight[which(NC$Tobacco == "Yes")])
observed <- NoTobaccoAvg - TobaccoAvg; observed # 215 ounces heavier without tobacco





