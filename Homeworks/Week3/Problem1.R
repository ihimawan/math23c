# setwd("Homeworks/Week3")
# cleanup
rm(list = ls())

# i've cleaned up this file to replace 'L-wo' or 'W-wo' with L and W respectively.
Mets <- read.csv("lib/Mets.csv"); head(Mets)
# obtain only W and L
Mets <- subset(Mets, Mets$W.L == "W" | Mets$W.L == "L" ); head(Mets)

p <- 1/4 # probability of winning
N <- nrow(Mets)
# A)

# barplot of theoretical lose until win
expected <- dgeom(0: N, p)
barplot(expected, names.arg = 0:N)

# barplot of obsertved lose until win ?????
tally <- numeric(N)
currentTally <- 1
for (i in 1:N){
  if (Mets$W.L[i] == "L"){
    currentTally <- currentTally + 1
  } else {
    tally[currentTally] =  tally[currentTally] + 1
    currentTally = 1
  }
}
observed <- tally/length(tally)
barplot(observed, names.arg = 1:N)

# ?????
barplot(rbind(observed, expected), beside = TRUE, col = c("red", "blue"))

#B) ?????

