# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

# A) Check answer with Question #4
m <- 6 # number of trumps
n <- 20 # number of non-trumps
h <- 13 # number on hand
dhyper(0:3, m, n, h) # [1] 0.007453416 0.072670807 0.242236025 0.355279503

# B) Estimate probability for East to have 3 trumps by using permutation test
k <- 3 # having 3 trumps on hand
Player <- c(rep("EAST", h), rep("WEST", m+n-h))
Trumps <- c(rep(TRUE,m), rep(FALSE,n))

N <- 10000
X <- numeric(N)
for (i in 1:N){
  scramble <- sample(Trumps, m+n)
  X[i] = sum(Player == "EAST" & scramble)
}

# finding the probability for k=3
table(X)[4]/N # answers around 0.3545

# C)
East <- c(rep(TRUE, h), rep(FALSE, m+n-h))
Trumps <- c(rep(TRUE, 2), rep(FALSE, n), rep(TRUE, m-2))
table(East, Trumps) # seeing that when East and Trumps is TRUE, it will be 2
fisher.test(East, Trumps, alternative = "g")
#
# 	Fisher's Exact Test for Count Data
#
# data:  East and Trumps
# p-value = 0.9199
# alternative hypothesis: true odds ratio is greater than 1
# 95 percent confidence interval:
#  0.04694746        Inf
# sample estimates:
# odds ratio
#  0.4234035
#
# probability of observing something as extreme as our table in the direction of our
# alternative hypothesis is 0.9199. Which means the probability East will be receiving
# at least 2 trumps would be 0.9199

# Checking with dhyper()
# To find P(X >= 2) = 1 - P(X < 2) = 1 - P(X <= 1)
1 - dhyper(1, m, n, h) # 0.9273292 which is close to earlier number