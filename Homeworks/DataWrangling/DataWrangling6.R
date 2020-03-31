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

AdmissionPredict <- read.csv("lib/Admission_Predict.csv")

# let's create the histogram of admission probability
hist(AdmissionPredict$Chance.of.Admit)
# looks like a beta distribution. With this, my hypothesis is that the chance of admission fits a gamma distriibution.

breaks <- hist(AdmissionPredict$Chance.of.Admit)$breaks
#  [1] 0.30 0.35 0.40 0.45 0.50 0.55 0.60 0.65 0.70 0.75 0.80 0.85 0.90 0.95 1.00
observed <- hist(AdmissionPredict$Chance.of.Admit)$count
#  [1]  2  5  9 19 15 24 48 43 63 55 34 34 38 11
# because of the low amount on bin 1, I will combine bin 1 and bin 2
observed[2] <- sum(observed[1:2])
observed <- observed[-1]
#  [1]  7  9 19 15 24 48 43 63 55 34 34 38 11
# Now the bins are x<0.4, 0.45-0.50, ... 0.95<x

nBins <- length(observed) # 13
pBin1 <- integrate(dexp, 0, 0.25)$value; 100*pBin1
pBin2 <- integrate(dexp, 0.25, 0.75)$value; 100*pBin2
pBin3 <- integrate(dexp, 0.75, 1.25)$value; 100*pBin3
pBin4 <- integrate(dexp, 1.25, Inf)$value; 100*pBin4