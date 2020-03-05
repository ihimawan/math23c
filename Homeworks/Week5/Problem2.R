# setwd("Homeworks/Week5")
# cleanup
rm(list = ls())

BodyTemp <- read.csv("lib/BodyTemp.csv"); head(BodyTemp)

# A) find expectation and variance
# WHOLE DATA SET
EX <- mean(BodyTemp$BodyTemp); EX # 98.24923 <- mean is 98.10462
EX2 <- mean(BodyTemp$BodyTemp^2); EX2 # 9653.445
Variance <- EX2 - EX^2; Variance # 0.5334225

# MEN
BodyTemp.men = BodyTemp$BodyTemp[which(BodyTemp$Gender == 'M')]
EX_men <- mean(BodyTemp.men); EX_men # 98.10462 <- mean is 98.10462
EX2_men <- mean(BodyTemp.men^2); EX2_men # 9624.996
Variance_men <- EX2_men - EX_men^2; Variance_men # 0.4807479

# FEMALE
BodyTemp.female = BodyTemp$BodyTemp[which(BodyTemp$Gender == 'F')]
EX_female <- mean(BodyTemp.female); EX_female # 98.39385 <- mean is 98.39385
EX2_female <- mean(BodyTemp.female^2); EX2_female # 9681.893
Variance_female <- EX2_female - EX_female^2; Variance_female # 0.5442698

# Seeing that the body temperature is relatively the same for both men and female

# B) permutation test to see significance
observed <- EX_female - EX_men; observed # female have 0.2892308 more temperature

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  Gender <- sample(BodyTemp$Gender)   # permuted gender column
  BodyTemp.men = BodyTemp$BodyTemp[which(Gender == 'M')]
  BodyTemp.female = BodyTemp$BodyTemp[which(Gender == 'F')]

  EX_men <- mean(BodyTemp.men)
  EX_female <- mean(BodyTemp.female)
  diffs[i] <- EX_female - EX_men
}


hist(diffs, breaks = "FD", probability = TRUE)
abline(v = observed, col = "red")
# looks pretty extreme

pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 0.0129987
pvalue.2tailed <- pvalue*2; pvalue.2tailed # around 0.0259974
# the two tailed p-value says there is only 0.0259974 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is the same between average body temperature of men and female

# C) overlay dnorm
EX_test <- mean(diffs) # 0.001776923, which is close to zero
EX2_test <- mean(diffs^2) # 0.01644404
curve(dnorm(x, mean = EX_test, sd = sqrt(EX2_test)), add=TRUE, col="green")
# matches well!