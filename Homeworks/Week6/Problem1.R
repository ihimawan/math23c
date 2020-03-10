# setwd("Homework/Week6")
# cleanup
rm(list=ls())

# Find det for this matirx
m <- cbind(c(1,1,0),c(1,1,2),c(1,0,1));m

# A) DEVELOPMENT BY FIRST COLUMN
my.det1 <- function(A) {
  if (length(A[,1]) == 2) return(A[1,1]*A[2,2]-A[1,2]*A[2,1])
  determ <- 0   #initialize the sum
  for (i in 1:length(A[,1])) {
    determ <- determ - ((-1)^i) * A[i,1] * my.det1(A[-i,-1])
  }
  determ
}
my.det1(m) # 2

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

my.det2(m) #2, same as above!

# C) COLUMN REDUCTION


