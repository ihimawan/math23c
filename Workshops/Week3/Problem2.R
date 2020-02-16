setwd("Workshops/Week3")
PopDensity <- read.csv("lib/PopDensity.csv")

# see if last digits are distributed uniformly
getLastDigit <- function (x) { x %% 10 }

N <- nrow(PopDensity)
NOfBuckets <- 5
buckets <- numeric(NOfBuckets)
for (i in 1:N) {
  lastDigit <- getLastDigit(PopDensity$X1096[i])
  bucketNo <- floor(lastDigit / 2) + 1
  buckets[bucketNo] <- buckets[bucketNo] + 1
}

observed <- buckets
barplot(observed) # barplot(expected)

expected <- rep(N/NOfBuckets, NOfBuckets)
chisq <- sum((observed - expected)^2/expected); chisq # 4.654412

#
#
#

# see if first digits are distributed uniformly
getFirstDigit <- function (x) { as.numeric(substr(as.character(x), 1, 1))}

N <- nrow(PopDensity)
NOfBuckets <- 3
buckets <- numeric(NOfBuckets)
for (i in 1:N) {
  firstDigit <- getFirstDigit(PopDensity$X1096[i])
  bucketNo <- ceiling(firstDigit / 3)
  buckets[bucketNo] <- buckets[bucketNo] + 1
}

observed <- buckets
barplot(observed) # does not look uniform at all

expected <- rep(N/NOfBuckets, NOfBuckets)
chisq <- sum((observed - expected)^2/expected); chisq # 178.5515


