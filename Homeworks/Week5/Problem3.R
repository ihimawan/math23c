# setwd("Homeworks/Week5")
# cleanup
rm(list = ls())

# A) generate 10^4 random numbers from Pareto distribution

randFx <- function(x) 4*(x)^(-5)
randParetoNbrs <- randFx(runif(10^4, min=1, max=5))
hist(randParetoNbrs, probability = TRUE, breaks = "FD")
curve(4*x^(-5), from = 0, to=5, add=TRUE)

# B) is Pareto distribution a good model
# TODO need buckets?

# I was unable to install the package (computer constantly stops working when installing).
# So I am using this as alternative:
load(file = "lib/danishuni.rda");
ds <- get("danishuni"); head(ds)

expected <- dpois(0:2, lambda);Expected[4]<-162*(1-ppois(3, lambda))
observed <- ds$Loss
hist(ds$Loss, probability = TRUE, breaks = "FD")
curve(4*x^(-5), from = 1, to=250, add=TRUE, color = "red")


