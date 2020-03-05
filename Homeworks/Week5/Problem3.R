# setwd("Homeworks/Week5")
# cleanup
rm(list = ls())

# A) generate 10^4 random numbers from Pareto distribution
library(EnvStats)
# randFx <- function(x) 4*(x)^(-5)
# we know that the location is 1 and shape 4 for this pareto function.
# So using that instaed of this function
randParetoNbrs <- rpareto(10^4,1,4)
hist(randParetoNbrs, probability = TRUE, breaks = "FD")
curve(dpareto(x, 1, 4), add=TRUE, col = "red", lwd = 3)
# matches well

# B) is Pareto distribution a good model

# I was unable to install the package (computer constantly stops working when installing).
# So I am using this as alternative:
load(file = "lib/danishuni.rda");
ds <- get("danishuni"); head(ds)

bins <- qpareto(0.1*(0:10), 1, 4)
loss <- ds$Loss
bincode <- cut(loss, breaks=bins)
bincode <- ifelse (loss==1, 1, bincode); bincode

# sanity check to make sure all data are counted for
sum(table(loss)) # 2167
length(loss) # 2167


observed <- as.vector(table(bincode))
expected <- rep(sum(observed)/10,10)
chisq <- sum((observed-expected)^2/expected); chisq # 4113.706 which is big

# 10 categories - 1 (because imposed actual and expected total) - 2 (imposed to parameters on data)
# = 7 degrees of freedom

pvalue <- pchisq(chisq, df = 7, lower.tail = FALSE); pvalue # 0
# Since it is zero, there is zero chance that the the data is from a Pareto distribution.
# We have enough evidence to say that this data is not of Pareto distribution.