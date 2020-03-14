# setwd("Homeworks/Week6")
# cleanup
rm(list=ls())

# # installs
# install.packages("combinat")
#
# libraries
library("combinat")

# Find det for this matirx
m <- matrix(c(1,4,5,3,-2,0,-1,2,3,1,2,1,0,2,1,0), nrow = 4)
#      [,1] [,2] [,3] [,4]
# [1,]    1   -2    3    0
# [2,]    4    0    1    2
# [3,]    5   -1    2    1
# [4,]    3    2    1    0
det(m) # 40 is the correct answer we can compare to

# A) DEVELOPMENT BY FIRST COLUMN
my.det1 <- function(A) {
  if (length(A[,1]) == 2) return(A[1,1]*A[2,2]-A[1,2]*A[2,1])
  determ <- 0   #initialize the sum
  for (i in 1:length(A[,1])) {
    determ <- determ - ((-1)^i) * A[i,1] * my.det1(A[-i,-1])
  }
  determ
}
my.det1(m) # 40! Correct!

# B) SUM OVER PERMUTATIONS
getSign <- function(perm) {
  n <- length(perm)
  names(perm) <- perm
  OutOfOrder <- outer(perm, perm, ">"); OutOfOrder
  OOO <- OutOfOrder * outer(1:n, 1:n, "<"); OOO
  sigma = sum(OOO[1:n, 1:n]) %% 2; sigma
  if (sigma == 0) 1
  else -1
}

getTerms <- function(matrix, perm){
  terms <- numeric(length = length(perm))
  for (row in 1:length(perm)){
    terms[row] <- matrix[row, perm[row]]
  }
  terms
}

# generating permutation
my.det2 <- function (matrix){
  permutations <- permn(1:length(matrix[,1]))
  sgn.term <- numeric(length = length(permutations))
  for (i in 1:length(permutations)){
    sign <- getSign(permutations[[i]])
    term <- getTerms(matrix, permutations[[i]])
    sgn.term[i] <- sign * prod(term)
  }
  sum(sgn.term)
}

my.det2(m) #40, same as above!

# C) COLUMN REDUCTION

m <- matrix(c(1,4,5,3,-2,0,-1,2,3,1,2,1,0,2,1,0), nrow = 4); m
det <- 1

# pivot first col
m[,2] <- m[,2] + 2*m[,1]
m[,3] <- m[,3] + (-3)*m[,1]
# pivot second col
m[,2] <- 1/8 * m[,2]; det <- det * 8
m[,3] <- m[,3] + 11 * m[,2]
m[,4] <- m[,4] + -2 * m[,2]
# pivot 3rd col
m[,3] <- -(1/0.625) * m[,3]; det <- det * (-0.625)
m[,4] <- m[,4] + (1.25) * m[,3]
# pivot 4th col
m[,4] <- -1/8 * m[,4]; det<-det * (-8); m
# Finally we get a triangular matrix
#      [,1]  [,2] [,3] [,4]
# [1,]    1 0.000  0.0    0
# [2,]    4 1.000  0.0    0
# [3,]    5 1.125  1.0    0
# [4,]    3 1.000 -4.8    1

det <- det * m[1,1] * m[2,2] * m[3,3] * m[4,4]; det #40 yay!