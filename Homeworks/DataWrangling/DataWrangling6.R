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
hist(AdmissionPredict$Chance.of.Admit)
# looks like a beta distribution. With this, my hypothesis is that the chance of admission fits a beta distriibution.
lambda <- mean(AdmissionPredict$Chance.of.Admit); lambda # 0.72435
curve(dbeta(x,10,1))
# https://rpubs.com/blakeobeans/fitdistrplus

# using ten bins
bins <- qgamma(0.1 * (0:10),  shape = r, rate = lambda)
#[1] 0.00000000 0.04322688 0.08320508 0.12585916 0.17329786 0.22791820 0.29336562 0.37624613
# [9] 0.49119244 0.68457743        Inf
bincode <- cut(Service$Times, bins, labels = FALSE); head(bincode)
# 10 10  9 10  7  9 (looks reasonable)