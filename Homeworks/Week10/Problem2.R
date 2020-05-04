# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file CigCancer.csv contains data about cigarette sales and the incidence
of various types of cancer in most of the states."
CigCancer <- read.csv("lib/CigCancer.csv"); head(CigCancer)

# get the columns of interest, put it in one
A <- cbind(CigCancer$CIG, CigCancer$BLAD, CigCancer$LUNG, CigCancer$KID, CigCancer$LEUK)
m <- nrow(A); m
n <- ncol(A); n
S <- var(A); S

"Center and scale the cancer data by the procedure described in Hub-
bard. This is tricky { look at how it was done for the weather data in
the lecture notes."

# center and scale data
A.phi <- scale(A, center = TRUE, scale = rep(sqrt(m-1),n))

mean(A.phi[,1]) # very close to 0, which is good
sum(A.phi[,1]^2); var(A[,1]) # both equals 31.06152 (sum of squares == variance)

"Use eigen() to get the four eigenvalues and the change of basis matrix.
Then add the coeficients of the four eigenvectors as new columns in
your data frame. Which of the eigenvectors correlates most strongly with cigarette smok-
ing? Can you put an interpretation on the eigenvector for the largest
eigenvalue? The second largest?"

Eig <- eigen(S); Eig

#              [,1]        [,2]        [,3]        [,4]        [,5]
# [1,]  0.822650816  0.55988987 -0.07352579  0.02792562  0.05985893
# [2,]  0.110317405 -0.01566851  0.69916557 -0.67906798 -0.19395557
# [3,]  0.556499695 -0.82672500 -0.02814014  0.07636419  0.01450877
# [4,]  0.035886437  0.04231738  0.16147703  0.42500880 -0.88893987
# [5,] -0.009795841  0.03182525  0.69202223  0.59297797  0.41033346

# eigenvectors
Eig$values

# matrix of eigenvectors
P <- Eig$vectors; P
Pinv <- solve(P); Pinv
Pinv %*% S %*% P

# we can see that this is actually a diagonal matrix of eigenvalues
round(Pinv %*% S %*% P)

# express A.phi relative to new basis of eigenvectors
A.eig <- A.phi %*% P; A.eig

#  coefficients of the new eigenvectors as new columns.
Cig.eig <- data.frame(CigCancer,
                      v1 = A.eig[,1],
                      v2 = A.eig[,2],
                      v3 = A.eig[,3],
                      v4 = A.eig[,4],
                      v5 = A.eig[,5])

Eig$values # first value is the largest, second value is second largest
Eig$vectors[,1] # first eivector correlates most strongly with sales of cigarettes

cor(CigCancer$CIG, A.eig[,1]) #  0.965105
cor(CigCancer$CIG, A.eig[,2]) # 0.2616235
cor(CigCancer$CIG, A.eig[,3]) # -0.009919685
cor(CigCancer$CIG, A.eig[,4]) # 0.002621214

# here the second eigenvalues & eigenvector is the second highest
# which correlates with cigarette sales. This makes sense because the first eigenvector
# is the highest level of variance in our data. Second is eigenvector is second highest, etc.

"Make sure that you can reconstruct the data exactly by using all five
eigenvalues, then see how well you can reconstruct it using just two
principal components. It took me a while to get this right { see the
lecture notes or script 10D."

# using all 5 eigenvalues
A2 <- cbind(A.eig[,1], A.eig[,2], A.eig[,3], A.eig[,4], A.eig[,5])

A.recon <- A2 %*% Pinv  # Multiply by Pinv on the right.
A.c <- scale(A.recon, scale = rep(1/sqrt(m-1),n)) # undoing scaling
AA <- scale(A.c, center = -colMeans(A), scale = FALSE) # undoing centering

# checking values
head(AA)
#      [,1] [,2]  [,3] [,4] [,5]
# [1,] 18.20 2.90 17.05 1.59 6.15
# [2,] 25.82 3.52 19.80 2.75 6.61
# [3,] 18.24 2.99 15.98 2.02 6.94
# [4,] 28.60 4.46 22.07 2.66 7.06
# [5,] 31.10 5.11 22.83 3.35 7.20
# [6,] 33.60 4.78 24.55 3.36 6.45

head(A)
#       [,1] [,2]  [,3] [,4] [,5]
# [1,] 18.20 2.90 17.05 1.59 6.15
# [2,] 25.82 3.52 19.80 2.75 6.61
# [3,] 18.24 2.99 15.98 2.02 6.94
# [4,] 28.60 4.46 22.07 2.66 7.06
# [5,] 31.10 5.11 22.83 3.35 7.20
# [6,] 33.60 4.78 24.55 3.36 6.45

# seeing the same numbers!

# now plot it..
plot(AA, A)
# looks good.

### now let's just use two principal components
A3 <- cbind(A.eig[,1], A.eig[,2], 0, 0, 0)

A.recon <- A3 %*% Pinv # Multiply by Pinv on the right.
A.c <- scale(A.recon, scale = rep(1/sqrt(m-1), n)) # undo scaling
AA <- scale(A.c, center = -colMeans(A), scale = FALSE) # Undo the centering.

# checking values
head(A)
#       [,1] [,2]  [,3] [,4] [,5]
# [1,] 18.20 2.90 17.05 1.59 6.15
# [2,] 25.82 3.52 19.80 2.75 6.61
# [3,] 18.24 2.99 15.98 2.02 6.94
# [4,] 28.60 4.46 22.07 2.66 7.06
# [5,] 31.10 5.11 22.83 3.35 7.20
# [6,] 33.60 4.78 24.55 3.36 6.45

head(AA)
#          [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 18.10794 3.359123 17.05072 2.467929 6.846901
# [2,] 25.75661 4.199055 19.75736 2.838212 6.834613
# [3,] 18.19095 3.284140 15.96297 2.488530 6.882439
# [4,] 28.57461 4.606386 22.05470 2.955337 6.788570
# [5,] 31.13770 4.877962 22.83035 3.081370 6.788647
# [6,] 33.57072 5.207906 24.52505 3.186780 6.758116

# seeing differences already

plot(A,AA)
# not as perfect as using 5 eigenvalues, but still decent