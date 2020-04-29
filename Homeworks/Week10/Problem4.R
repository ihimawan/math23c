# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file Forbes.csv contains nancial data on a number of large U.S. com-
panies. Bind the six numeric columns into a matrix, and then use var()
to construct the covariance matrix A. (The point is just to get a large
symmetric matrix based on real-world data.)"

"(a) Use eigen() to nd the eigenvectors and eigenvalues of A."

"(b) Find the largest eigenvalue of A, and the correponding eigenvector,
by searching for the maximum of QA on the unit sphere in R6. Use
the approach of R workshop problem 5. You already know the right
answer; so you can tell when your optimization algorithm is good
enough."

"(c) Construct the matrix I - P that projects onto the space orthogonal
to the first eigenvector"

"(d) Find the second largest eigenvalue and the corresponding eigenvec-
tor by searching for the maximum of QA on the set of unit vectors
that are orthogonal to your rst eigenvector. Hint: apply I - P to
your multivariate standard normal vectors before converting to a unit
vector."