# setwd("Homeworks/Week4")
# cleanup
rm(list = ls())

# A) find that kurtosis is 0
mu <- 0

# find variance
Var <- integrate(function (x) (x-mu)^2 * dnorm(x, mu), -Inf, Inf); Var
# 1 with absolute error < 1.2e-07

# find mu_4
Mu_4 <- integrate(function (x) (x-mu)^4 * dnorm(x, mu), -Inf, Inf); Mu_4
# 3 with absolute error < 5.7e-06

# find kurtosis
kurtosis <- Mu_4$value / Var$value^2 - 3; kurtosis
# 1.625367e-13, which is very close to zero

# B) find variance and kurtosis for uniform distribution
start <- -1
end <- 1
mu <- 1/2

# find variance
#
Var <- integrate(function (x) (x-mu)^2 * dunif(x), start, end); Var
# 0.08333333 with absolute error < 9.2e-16

# find mu_4
Mu_4 <- integrate(function (x) (x-mu)^4 * dunif(x), start, end); Mu_4
# 0.0125 with absolute error < 1.4e-16

# find kurtosis
kurtosis <- Mu_4$value / (Var$value)^2 - 3; kurtosis
# Kurtosis is -1.2