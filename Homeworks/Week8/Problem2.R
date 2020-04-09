# setwd("Homeworks/Week7")
# cleanup
rm(list=ls())

f <- function(x) dt(x,1)
integrate(f, -Inf, Inf) # 1 with absolute error < 1.6e-10

"Show that for 1 degree of freedom, attempting to calculate the variance
# in R leads to a \"probably divergent\" integral."

# E((x-mu)^2) = E(X^2) - (E(X))^2

# take the expectation
ex.df1 <- function (x) x*dt(x, 1)
integrate(ex.df1, -Inf, Inf)$value
# results in 0 (which is wrong) because it thinks -Inf + Inf = 0, which is wrong.

# take second moment
ex2.df1 <- function (x) x^2*dt(x, 1)
integrate(ex2.df1, -Inf, Inf)$value
# Exception: the integral is probably divergent
# E(X^2) doesn't exist. So variance does not exist.

"Use the formula from item 20 to show that R is correct."
formula.df1 <- function (x) (1/pi) * (x/(1+x^2))
integrate(formula.df1, -Inf, Inf)$value # 0 which is wrong

formula2.df1 <- function (x) (1/pi) * (x^2/(1+x^2))
integrate(formula2.df1, -Inf, Inf)$value # probably divergent, so the variance also doesn't exist

"For 1 degree of freedom, show that attempting to calculate the ex-
pectation in R leads to a value of zero. This is wrong;"

integrate(ex.df1, -Inf, Inf)$value # 0 which is wrong.

"the improper integral does not exist. Confirm this in three ways:
# 1. Do the integral from 0 to inf or from -inf to 0: it is \"probably
# divergent.\""

integrate(ex2.df1, -Inf, 0) # "probably divergent"
integrate(ex2.df1, 0, Inf) # "probably divergent"

# the sum Inf and -Inf is not zero and cannot add 2 divergent integrals to get a divergent integral.
# Therefore, the expectation is not zero.

"2. Make a histogram of sample means for samples of size 1000 and
observe that the sample mean is not reliably close to zero."

trials <- 10^4
sampleSize <- 10^3
Xbar <- numeric(trials)

for (i in 1:trials){
  Xbar[i] <- mean(sample(rt(sampleSize, df = 1)))
}

hist(Xbar, probability = TRUE, breaks="FD")
# from the graph, we can see that the range is large, which means
# it is not reliably close to zero.

"Do the expectation calculation using large but unequal limits of
integration, like -100 and 200. You will get a number that is not
close to zero."

integrate(ex.df1, -100, 200)$value # 0.2206237
integrate(ex.df1, -300, 1000)$value  # 0.3832348
integrate(ex.df1, -1000, 4000)$value # 0.4412711
integrate(ex.df1, -4000, 5000)$value # 0.07102879
# they are not close to zero, however.

"For 2 degrees of freedom, use R to show that the expectation really is
zero by doing the integral from 0 to 1 and from 􀀀1 to 0. Make a
histogram of sample means for samples of size 1000 and observe that
the sample mean is reliably close to zero. Then show that attempting
to calculate the variance in R fails. Use the formula from item 20 to
explain why."

ex.df2 <- function (x) x * dt(x,2)
integrate(ex.df2, -Inf, Inf)$value # 0, which is correct.

integrate(ex.df2, -Inf, 0)$value # -0.7071068
integrate(ex.df2, 0, Inf)$value # 0.7071068
# both values are not divergent, and the sum would be zero.
# therefore the integral converges to 0 (correctly, this time.)

trials <- 10^4
sampleSize <- 10^3
Xbar <- numeric(trials)

for (i in 1:trials){
  Xbar[i] <- mean(sample(rt(sampleSize, df = 2)))
}


hist(Xbar, probability = TRUE, breaks="FD")
# from the graph, we can see that the range is very small (around -1.0 < x < 1.0 in this trial), which means
# it is reliably close to zero.

ex2.df2 <- function (x) x^2 * dt(x,2)
integrate(ex2.df2, -Inf, Inf)$value # "maximum number of subdivisions reached"
# R attempted to find it, but it does not converge.
# therefore the second moment (hence, the variance) does not exist.

# now, use item 20 formula
formula2.df2 <- function(x) x^2/(2+x^2)^(3/2)
integrate(formula2.df2, -Inf, Inf)$value # does not converge
# therefore the second moment (hence, the variance) does not exist.

"For 3 degrees of freedom, use R to show that the variance is 3"
ex.df3 <- function(x) x*dt(x,3)
EX <- integrate(ex.df3, -Inf, Inf)$value; EX #0

ex2.df3 <- function(x) x^2*dt(x,3)
EX2 <- integrate(ex2.df3, -Inf, Inf)$value; EX2 #3

variance.df3 <- EX2 - EX^2;  variance.df3 # 3; it is indeed 3!

"and confirm the result by using the substitution t = sqrt(3)tan(u) to evaluate the integral"
# .pdf will be attached

"For 4 degrees of freedom, evaluate the gamma functions in the general
formula to get a formula of the form: mu_T(t) = c/(4+t^2)^(5/2)"

#  integrating 1/(4+t^2)^(5/2) will be 1/c
muT <- function(x) 1/(4 + x^2)^(5/2)
recip <- integrate(muT, -Inf, Inf)$value; recip # 0.08333333
1/recip #12

"show that variance is 2"
ex.formula <- function(x) (12*x)/(4 + x^2)^(5/2)
EX <- integrate(ex.formula, -Inf, Inf)$value; EX #0
ex2.formula <- function(x) (12*x^2)/(4 + x^2)^(5/2)
EX2 <- integrate(ex2.formula, -Inf, Inf)$value; EX2 #2

variance <- EX2 - EX^2; variance # 2 (variance is 2!)

"For k degrees of freedom, the variance is k/(k-2) : Use R to confirm this
for k = 12"

ex.formula <- function(x) x*dt(x,12)
EX <- integrate(ex.formula, -Inf, Inf)$value; EX #0
ex2.formula <- function(x) x^2*dt(x,12)
EX2 <- integrate(ex2.formula, -Inf, Inf)$value; EX2 #1.2
variance <- EX2 - EX^2; variance # 1.2

# using k = 12
k <- 12
k/(k-2) # 1.2 (matches!)