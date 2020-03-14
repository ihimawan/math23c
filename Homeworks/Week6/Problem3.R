# setwd("Homeworks/Week6")
# cleanup
rm(list=ls())

SAT <- read.csv("lib/SATScores.csv")

# install.packages("MASS")
library(MASS)

# a) scatter plot
plot(SAT$Math, SAT$Verbal, pch = ".", cex =3 , asp = 1)

# b) means
mu.math <- mean(SAT$Math); mu.math # 613.4737
mu.verbal <- mean(SAT$Verbal); mu.verbal # 596.8421

# c) covariance matrix for mvrnorm()
# cor (x,y) = cov(x,y) / (sd(x) * sd(y))
# so cov(x,y) = cor(x,y) * sd(x) * sd(y)

cov(SAT) # built-in cov function for checking
#            Math   Verbal
# Math   6426.271 2170.515
# Verbal 2170.515 6073.795

Math <- SAT$Math; Verbal <- SAT$Verbal
entry11 <- cor(Math, Math) * sd(Math) * sd(Math); entry11 # 6426.271 (matches)
entry12 <- cor(Math, Verbal) * sd(Math) * sd(Verbal); entry12 # 2170.515 (matches)
entry21 <- cor(Verbal, Math) * sd(Verbal) * sd(Math); entry21 # 2170.515 (matches)
entry22 <- cor(Verbal, Verbal) * sd(Verbal) * sd(Verbal); entry22 # 6073.795 (matches)

sigma <- rbind(c(entry11, entry12), c(entry21, entry22)); sigma
#          [,1]     [,2]
# [1,] 6426.271 2170.515
# [2,] 2170.515 6073.795
#
# MATCHES!

# d) generate 300 samples
samples <- mvrnorm(n = 300, mu = c(mu.math, mu.verbal), Sigma = sigma)

my.Math <- samples[,1]
my.Verbal <- samples[,2]

points(my.Math, my.Verbal, pch = ".", cex=3, col="red")
# Looks like a good fit!!