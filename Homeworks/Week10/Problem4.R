# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file Forbes.csv contains nancial data on a number of large U.S. com-
panies. Bind the six numeric columns into a matrix, and then use var()
to construct the covariance matrix A. (The point is just to get a large
symmetric matrix based on real-world data.)"

Forbes <- read.csv("lib/Forbes.csv")

m <- nrow(Forbes);m
#Make a matrix of six of the columns.
A <- cbind(Forbes$Assets, Forbes$Sales,Forbes$Market_Value,Forbes$Profits, Forbes$Cash_Flow, Forbes$Employees)
#Make the covariance matrix
S <- var(A); S

"(a) Use eigen() to find the eigenvectors and eigenvalues of A."

eigen(S)

# eigen() decomposition
# $values
# [1] 2.230162e+08 3.229475e+07 7.445280e+06 1.091608e+05 6.577009e+03 4.864228e+02
#
# $vectors
#              [,1]         [,2]        [,3]         [,4]         [,5]          [,6]
# [1,] -0.524048396  0.831979978 -0.18190130 -0.009262657  0.002558655  0.0014719774
# [2,] -0.439063135 -0.080995701  0.89462543 -0.001017459  0.014067432 -0.0104721349
# [3,] -0.724379629 -0.543193636 -0.40431652  0.128541619 -0.014820931 -0.0006091338
# [4,] -0.048056346 -0.047768599 -0.04251890 -0.507911115  0.856321532 -0.0483800085
# [5,] -0.074488379 -0.062473513 -0.03465773 -0.851659035 -0.512267185  0.0400824507
# [6,] -0.003614418 -0.002215186  0.00873997  0.009664752  0.062222712  0.9979682202


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