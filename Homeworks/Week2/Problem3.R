# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

NC <- read.csv("lib/NCBirths2004_.csv");

# Testing difference between mean weight is significant
MaleAvg <- mean(NC$Weight[which(NC$Gender == "Male")])
FemaleAvg <- mean(NC$Weight[which(NC$Gender == "Female")])
observed <- MaleAvg - FemaleAvg # Males are 103.26 ounces heavier

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(NC$Gender)   # permuted gender column
  MaleAvg <- mean(NC$Weight[which(Gender=="Male")])
  FemaleAvg <- mean(NC$Weight[which(Gender=="Female")])
  diffs[i] <- MaleAvg - FemaleAvg
}

mean(diffs)
hist(diffs, breaks = "FD")
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 0.00059994
pvalue.2tailed <- pvalue*2; pvalue.2tailed # around 0.00119988
# the two tailed p-value says there is only 0.00119988 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is no difference in mean birth weights and toward the alternative hypothesis
# that there is a difference. Shown by how Male babies are heavier.

# Testing significance of tobacco with birth weight
NoTobaccoAvg <- mean(NC$Weight[which(NC$Tobacco == "No")])
TobaccoAvg <- mean(NC$Weight[which(NC$Tobacco == "Yes")])
observed <- NoTobaccoAvg - TobaccoAvg; observed # 215.0021 ounces heavier without tobacco

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Tobacco <- sample(NC$Tobacco)   # permuted Tobacco column
  NoTobaccoAvg <- mean(NC$Weight[which(Tobacco=="No")])
  TobaccoAvg <- mean(NC$Weight[which(Tobacco=="Yes")])
  diffs[i] <- NoTobaccoAvg - TobaccoAvg
}

mean(diffs)
hist(diffs, breaks = "FD")
abline(v = observed, col = "red")
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # small number around 9.999e-05
pvalue.2tailed <- pvalue * 2; pvalue.2tailed # small number around .00019998
# the two tailed p-value says there is only 0.00019998 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is no difference in mean birth weights and toward the alternative hypothesis
# that there is a difference. Shown by how babies whose moms don't use tobacco
# is heavier.