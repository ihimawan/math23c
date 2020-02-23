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

# B) find variance and kurtosis for uniform distribution ???
# JUST GUESSING TODO NEED TO CHECK
start <- -1
end <- 1
mu <- 1/2

# find variance

y <- function(x) dunif(mu, -1,1)
# regular integration
Var <- integrate(Vectorize(y), start, end); Var

Var <- integrate(function (x) (x-mu)^2 * dunif(mu, start, end), start, end); Var
# 0.5833333 with absolute error < 6.5e-15

# find mu_4
Mu_4 <- integrate(function (x) (x-mu)^4 * dnorm(x, mu), -Inf, Inf); Mu_4
# 3 with absolute error < 4.2e-06

# find kurtosis
kurtosis <- Mu_4$value / Var$value^2 - 3; kurtosis
# 5.816327