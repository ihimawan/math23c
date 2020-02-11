# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

answer = c(1, 0, 0, 0)
quizzes <- expand.grid(Q1 = answer, Q2 = answer); quizzes

# X is average quiz score, 100, 50, 0
quizzes$X <- (quizzes$Q1 + quizzes$Q2) / 2; quizzes
# Y is improvement rating
improvement <- function(q1, q2) ifelse(q2 < q1, 0, ifelse(q2 == q1, 1, 2))
quizzes$Y <- improvement(quizzes$Q1, quizzes$Q2); quizzes

# E(X)E(Y) == E(XY)
mean(quizzes$X) * mean(quizzes$Y) == mean(quizzes$X * quizzes$Y) # TRUE
# X and Y are uncorrelated

# E(X^2Y^2) == E(X^2)E(Y^2)
mean(quizzes$X^2 * quizzes$Y^2) == mean(quizzes$X^2) * mean(quizzes$Y^2) # FALSE
# X and Y are not independent

# event A <- event where X = 0.5
# event B <- event where Y = 2
# event C <- event where X = 0.5 and Y = 2
pA <- length(which(quizzes$X == 0.5))/16; pA
pB <- length(which(quizzes$Y == 2))/16; pB
pC <- length(which(quizzes$X == 0.5 & quizzes$Y == 2))/16; pC
pC == pA * pB #FALSE

