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

# recursive way to find matrix determinant
my.det <- function(A) {
  if (length(A[,1]) == 2){
    return(A[1,1]*A[2,2]-A[1,2]*A[2,1])
  } else {
    det <- 0
    for (i in 1:length(A[1,])){
      det <- det - ((-1)^i) *
        A[i, 1] * # which col
        my.det(A[-i, -1]) #  the determinant of the bottom with row i and col 1 removed
    }
    det
  }
}

# testing out the method
m <- cbind(c(1,1,0),c(1,1,2),c(1,0,1));m
my.det(m)
m <- cbind(c(1,2),c(2,1));
my.det(m)


