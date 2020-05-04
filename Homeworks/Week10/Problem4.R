# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

Forbes <- read.csv("lib/Forbes.csv")

m <- nrow(Forbes);m
#Make a matrix of six of the columns.
A <- cbind(Forbes$Assets, Forbes$Sales,Forbes$Market_Value,Forbes$Profits, Forbes$Cash_Flow, Forbes$Employees)
#Make the covariance matrix
S <- var(A); S

Eig <- eigen(S); Eig

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

Eig$vectors # with eigenvectors
Eig$values # and eigenvalues

QA <- function(x) {sum(x * (A %*% x))}

# find largest eigenvector and its eigenvalue
# by searching the unit sphere in R^6.

# Run 100000 trials.
N <- 10^5

# Create a random vector, Qval, to store the values of the function QA.
# Then store the corresponding vectors to Xval as an N-component list.
Qval <- numeric(N); Xval <- list(N)

# Run the loop.
for (i in 1:N) {
  x <- runif(6, min = -1, max = 1) #uniformly generated components from -1 to 1
  x1 <- x/sqrt(sum(x*x)) # normalize
  Qval[i] <- QA(x1)
  Xval[[i]] <- x1
}

# Take maximum function value and compare to the maximum eigenvalue
# from the eigen function, which should be similar
lambda1 <- max(Qval); lambda1
Eig$values[1]

# corresponding eigenvector and Compare with the eigen function, which is close
v1 <- Xval[[which.max(Qval)]]; v1
Eig$vectors[,1]

# We can also check directly that we've
# found an eigenvector/eigenvalue using the definition, which is close
A %*% v1; lambda1 * v1

# Construct the matrix I - P that projects onto the space orthogonal to v1.

P <- v1%*%solve(t(v1) %*% v1)%*%t(v1); P
I <- diag(6); I
IlessP <- I - P; IlessP

# Now, we find the second largest eigenvalue and its corresponding eigenvector
# by searching the orthogonal subspace.

# Number of trials
N <- 10^5

# Store QA values and vectors
Qval <- numeric(N); Xval <- list(N)

# Run the loop
for (i in 1:N) {
  x <- rnorm(6) # sample of size 6
  Ax <- IlessP %*% x
  u <- Ax/sqrt(sum(Ax*Ax)) # normalize
  Qval[i] <- QA(u)
  Xval[[i]] <- u
}

# Largest eigenvalue, would be close
lambda2 <- max(Qval); lambda2
Eig$values[2]

# Corresponding eigenvector and eigen function, close
v2 <- Xval[[which.max(Qval)]]; v2
Eig$vectors[,2]

# Close match.

# directly compare yields close match
A%*%v2; lambda2*v2