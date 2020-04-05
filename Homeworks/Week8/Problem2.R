# setwd("Homeworks/Week7")
# cleanup
rm(list=ls())

# Show that for 1 degree of freedom, attempting to calculate the variance
# in R leads to a \probably divergent" integral. Use the formula from
# item 20 to show that R is correct.

# For 1 degree of freedom, show that attempting to calculate the ex-
# pectation in R leads to a value of zero. This is wrong; the improper
# integral does not exist. Confirm this in three ways:

# 1. Do the integral from 0 to inf or from -inf to 0: it is "probably
# divergent."

# 2. Make a histogram of sample means for samples of size 1000 and
# observe that the sample mean is not reliably close to zero.

# 3. Do the expectation calculation using large but unequal limits of
# integration, like -100 and 200. You will get a number that is not
# close to zero.


# For 2 degrees of freedom, use R to show that the expectation really is
# zero by doing the integral from 0 to 1 and from 􀀀1 to 0. Make a
# histogram of sample means for samples of size 1000 and observe that
# the sample mean is reliably close to zero. Then show that attempting
# to calculate the variance in R fails. Use the formula from item 20 to
# explain why.

# For 3 degrees of freedom, use R to show that the variance is 3, and
# conrm the result by using the substitution t = sqrt(3)tan(u) to evaluate the integral

# For 4 degrees of freedom, evaluate the gamma functions in the general
# formula to get a formula of the form:

# For k degrees of freedom, the variance is k/(k-2) : Use R to conrm this
# for k = 12