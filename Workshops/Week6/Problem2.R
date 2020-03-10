# setwd("Workshop/Week6")
# cleanup
rm(list=ls())

# # installs
# install.packages("MASS")
# install.packages("combinat")
#
# # libraries
# library("MASS")
# library("combinat")

# evaluate determinant as sum over permutations

# function to get sign
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
my.det <- function (matrix){
  permutations <- permn(1:length(matrix[,1]))
  sgn.term <- numeric(length = length(permutations))
  for (i in 1:length(permutations)){
    sign <- getSign(permutations[[i]])
    term <- getTerms(matrix, permutations[[i]])
    sgn.term[i] <- sign * prod(term)
  }
  sum(sgn.term)
}

m <- cbind(c(1,1,0),c(1,1,2),c(1,0,1))
my.det(m) #2
m <- cbind(c(1,1,1),c(1,1,0),c(0,2,1))
my.det(m) #2
m <- cbind(c(1,2),c(2,1))
my.det(m) #-3
