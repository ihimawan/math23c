# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

Mets <- read.csv("lib/Mets.csv")

p <- 1/4 # probability of winning
N <- nrow(Mets)

# A) tally up numbers and display side by side
wins <- Mets$R > Mets$RA
indexOfWins <- which(wins); # there is 40 of this
losesUntilWins <- numeric(40)
losesUntilWins[1] <- indexOfWins[1]

# populate the rest with a loop
for (i in 2:40) {
  losesUntilWins[i] <- indexOfWins[i] - indexOfWins[i-1]
}

# in order to run chi squared test, we need to have 5 to run chi squared test on each bucket
# The buckets will be separated as
# bucket 1 = 1
# bucket 2 = 2
# bucket 3 = 3 OR 4
# bucket 4 = >=5
observed <- table(losesUntilWins)
len <- length(observed)
observed[3] <- sum(observed[3:4])
observed[4] <- sum(observed[5:len])
observed <- observed[-(5:len)]
#  1  2  3  4
# 11 10  7 12
# sum(observed) is 40

# finding expected matching above bucket allocations
expected <- 40 * c(dgeom(0: 1, p),
                   dgeom(2, p) + dgeom(3,p),
                   1 - pgeom(3, p))
# get the barplot
barplot(rbind(observed, expected), beside = TRUE, col = c("red", "blue"))

# calculate chi sq test
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- ChiSq(observed, expected); chisq # 1.788889
# degrees of freedom is 4-2 since we expected total equal to the actual total
# and computed p from data
pValue <- pchisq(chisq, 2, lower.tail = FALSE); pValue # 0.4088347
# This is saying the probability of this happening is 0.4088347
# There is not enough evidence to reject the null hypothesis that
# the data is a geometric distribution

# B) using Box-Jenkins
streak <- numeric(nrow(Mets))
streak[1] <- min(which(wins))

for (i in 2:nrow(Mets)){
  streak[i] <- min(which(tail(wins, 1-i)))
}

# doing 5 bins 1,2,3,4, >=5
observed <- numeric(5)
# populate first 4 entries
observed[1:4] <- table(streak)[1:4]
observed[5] <- sum(table(streak)[5:length(table(streak))])
# sum(observed) # confirmed that all of this sums up to 161

# according to R, our bins are 0, 1,2,3, >=4
N <- nrow(Mets); N
expected <- N*c(dgeom(0:3, p), pgeom(3, p, lower.tail = FALSE))
sum(expected) == N #TRUE

ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- ChiSq(observed, expected); chisq # 1.410858
# computed a parameter from the data and made the expected total equal to the actual total,
# so we lose 2 degrees of freedom.
pValue <- pchisq(chisq, 5-2, lower.tail = FALSE); pValue # 0.7029915
# This is saying the probability of this happening is 0.7029915
# we do not have enough evidence to reject the null hypothesis
# that the data is a geometric distribution.
