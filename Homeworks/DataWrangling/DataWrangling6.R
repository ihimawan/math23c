# Find  on  the  Internet  some  data  that  might  plausibly  have  been  sampled from  a
# continuous  distribution  (exponential,  gamma,  chi  square,  beta).
# Dispaly  a  histogram  of  your  data  and  overlay  on  it  a  graph  of  the
# den-sity function for your hypothesized distribution.  Then use integration is R
# to compute the expected number of counts in various bins,  and compare with the observed
# number of counts.  The prototype is the analysis for the exponential distribution in script
# 4D, but there the data were preassigned to bins.
#
# This dataset is created for prediction of Graduate Admissions from an Indian perspective.
# https://www.kaggle.com/mohansacharya/graduate-admissions
# Mohan S Acharya, Asfia Armaan, Aneeta S Antony : A Comparison of Regression Models for Prediction of Graduate Admissions, IEEE International Conference on Computational Intelligence in Data Science 2019

# setwd("Homeworks/DataWrangling")
rm(list = ls())

# use fitdistrplus
install.packages(fitdistrplus)
library(fitdistrplus)

AdmissionPredict <- read.csv("lib/Admission_Predict.csv")

# let's create the histogram of admission probability
hist(AdmissionPredict$Chance.of.Admit, probability = TRUE)
# looks like a beta/gamma distribution.

# using the library, I want to see if it's closed to a beta/gamma distribution
# https://rpubs.com/blakeobeans/fitdistrplus
?fitdist
fit_b  <- fitdist(AdmissionPredict$Chance.of.Admit, "beta")
shape1 <- 6.099548
shape2 <- 2.303979
fit_g  <- fitdist(AdmissionPredict$Chance.of.Admit, "gamma")
shape <- 23.43407
rate <-  32.35186

curve(dbeta(x,shape1,shape2), add = TRUE, col="red")
curve(dgamma(x,shape = shape,rate=rate), add = TRUE, col="green")
# To me, there's more likeness to the gamma distribution.
# I'll be using that as my hypothesis, that the chance of admission is a gamma distribution.

breaks <- hist(AdmissionPredict$Chance.of.Admit)$breaks
#  [1] 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00
observed <- hist(AdmissionPredict$Chance.of.Admit)$count
# [1]  2  5  9 19 15 24 48 43 63 55 34 34 38 11
# nice mostly large numbers. However, I will be combining bin 1 and bin 2
observed[2] <- sum(observed[1:2])
observed <- observed[-1]
# [1]  7  9 19 15 24 48 43 63 55 34 34 38 11
# with bins 0 < x < 0.4, 0.4 < x < 0.45 .... 0.95 < x

numOfObservations <- sum(observed) # 400
expected <- numOfObservations*c(pgamma(0.40, rate),
                                pgamma(0.40, rate)-pgamma(0.45, rate),
                                pgamma(0.45, rate)-pgamma(0.50, rate),
                                pgamma(0.50, rate)-pgamma(0.55, rate),
                                pgamma(0.55, rate)-pgamma(0.60, rate),
                                pgamma(0.60, rate)-pgamma(0.65, rate),
                                pgamma(0.65, rate)-pgamma(0.70, rate),
                                pgamma(0.70, rate)-pgamma(0.75, rate),
                                pgamma(0.75, rate)-pgamma(0.80, rate),
                                pgamma(0.80, rate)-pgamma(0.85, rate),
                                pgamma(0.85, rate)-pgamma(0.90, rate),
                                pgamma(0.90, rate)-pgamma(0.95, rate),
                                1-pgamma(0.95, rate))
# sanity check
sum(expected) # 400! yay

n <- length(expected)
#Test for uniformity using chi square.
chisq <- sum((observed - expected)^2/expected); chisq #  1.157089e+48
# We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = n-2, lower.tail = FALSE); pValue # 0
# since p-value is zero.. we reject the null hypothesis that the distribution comes from a gamma distribution..