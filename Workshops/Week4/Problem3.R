# setwd("Workshop/Week4")
# cleanup
rm(list = ls())

head(Turbine)
shape <- 8
r <- 1.18

hist(Turbine$AveSpeed, breaks="FD", probability = TRUE)
curve(dgamma(x, shape = shape, rate = r), add = TRUE, col="red")
# It’s amazing what you can dowith two free parameters!

# Use  the  quantile  function  to  get  the  boundaries  for  10  bins,
# each  with  10%  of  the probability.
bins <- qgamma(0.1 * (0:10), shape = shape, rate = r)

# Use cut(), as at the end of the Week 4 lecture notes,
# to assign a bin code to each datavalue.
bincode <- cut(Turbine$AveSpeed, bins, labels = FALSE)

Observed <- table(bincode); Observed
Expected <- sum(Observed)/10
chisq <-sum((Observed-Expected)^2/Expected); chisq #  6.761905
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 0.4540842
chisq.test(Observed)
#
# 	Chi-squared test for given probabilities
#
# data:  Observed
# X-squared = 6.7619, df = 9, p-value = 0.6619
#
# there is 0.6619 chance that the observed discrepancy is by chance.
# If we are using the significance level on .05, we don't have enough
# evidence against the null hypothesis that there
# is no difference between the observed graph and the gamma distribution;
# towards the observed graph to be similar to gamma distribution

#### NOW USING 8 BINS! ####

# Use  the  quantile  function  to  get  the  boundaries  for  8  bins,
# each  with  12.5%  of  the probability.
bins <- qgamma(0.125 * (0:8), shape = shape, rate = r)

# Use cut(), as at the end of the Week 4 lecture notes,
# to assign a bin code to each datavalue.
bincode <- cut(Turbine$AveSpeed, bins, labels = FALSE)

Observed <- table(bincode); Observed
Expected <- sum(Observed)/8
chisq <- sum((Observed-Expected)^2/Expected); chisq # 3.333333
pValue <- pchisq(chisq, df = 5, lower.tail = FALSE); pValue # 0.6487424
chisq.test(Observed)
#
# 	Chi-squared test for given probabilities
#
# data:  Observed
# X-squared = 3.3333, df = 7, p-value = 0.8526
#
# there is 0.8526 chance that the observed discrepancy is by chance.
# If we are using the significance level on .05, we don't have enough
# evidence against the null hypothesis that there
# is no difference between the observed graph and the gamma distribution;
# towards the observed graph to be similar to gamma distribution

#### NOW WITH 12 BINS! ####

# Use  the  quantile  function  to  get  the  boundaries  for  12  bins,
# each  with  1/12  of  the probability.
bins <- qgamma((1/12) * (0:12), shape = shape, rate = r)

# Use cut(), as at the end of the Week 4 lecture notes,
# to assign a bin code to each datavalue.
bincode <- cut(Turbine$AveSpeed, bins, labels = FALSE)

Observed <- table(bincode); Observed
Expected <- sum(Observed)/12
chisq <- sum((Observed-Expected)^2/Expected); chisq # 11.14286
pValue <- pchisq(chisq, df = 9, lower.tail = FALSE); pValue # 0.2660438
chisq.test(Observed)
#
# 	Chi-squared test for given probabilities
#
# data:  Observed
# X-squared = 11.143, df = 11, p-value = 0.4314
#
# there is 0.4314 chance that the observed discrepancy is by chance.
# If we are using the significance level on .05, we don't have enough
# evidence against the null hypothesis that there
# is no difference between the observed graph and the gamma distribution;
# towards the observed graph to be similar to gamma distribution