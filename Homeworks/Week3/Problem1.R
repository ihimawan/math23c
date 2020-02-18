# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

Mets <- read.csv("lib/Mets.csv"); head(Mets)

p <- 1/4 # probability of winning
N <- nrow(Mets)

# A) tally up numbers and display side by side
wins <- Mets$R > Mets$RA
indexOfWins <- which(wins); indexOfWins
X <- numeric(40)
X[1] <- indexOfWins[1]

# populate the rest with a loop
for (i in 2:40) {
  X[i] <- indexOfWins[i] - indexOfWins[i-1]
}

# in order to run chi squared test, we need to have 5 to run chi squared test
observed <- table(X)
observed[4] <- sum(observed[4:12])
observed <- observed[-(5:12)]
observed <- as.vector(observed)

expected <- 40 * c(dgeom(0: 1, p), dgeom(2, p) + dgeom(3,p), 1 - pgeom(3, p))
# get the barplot
barplot(rbind(observed, expected), beside = TRUE, col = c("red", "blue"))

# calculate chi sq test
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- ChiSq(observed, expected); chisq # TODO
# degrees of freedom is 4-2
pValue <- pchisq(chisq, 2, lower.tail = FALSE); # TODO:  needs to be 40.88

# B) using Box-Jenkins
streak <- numeric(nrow(Mets)); length(streak)
streak[1] <- min(which(wins)); head(streak)

for (i in 2:nrow(Mets)){
  streak[i] <- min(which(tail(wins, 1-i)))
}

table(streak)

# lets do 5 bins 1,2,3,4, >=5
observed <- numeric(5)
# populate first 4 entries
observed[1:4] <- table(streak)[1:4]; observed
observed[5] <- sum(table(streak)[5:length(table(streak))])
sum(observed) # must be 161

# according to R, our bins are 1,2,3, >=4
N <- nrow(Mets); N
expected <- N*c(dgeom(0:3, p), pgeom(3, p, lower.tail = FALSE))
sum(expected) == N #true

ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

chisq <- ChiSq(observed, expected) # TODO 1.410858
# computed a parameter from the data and made the expected total equal to the actual total,
# so we lose 2 degrees of freedom.
pValue <- pchisq(chisq, 5-2, lower.tail = FALSE);# TODO 0.7029915 (fail to reject it came from geometric distribution)
