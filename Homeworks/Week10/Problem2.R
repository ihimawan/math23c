# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file CigCancer.csv contains data about cigarette sales and the incidence
of various types of cancer in most of the states."
CigCancer <- read.csv("lib/CigCancer.csv"); head(CigCancer)

"Center and scale the cancer data by the procedure described in Hub-
bard. This is tricky { look at how it was done for the weather data in
the lecture notes."

nEigen <- 4
nCancer <- nrow(CigCancer)/11;nCancer

extract <- function(x) CigCancer$CIG[(1+nEigen*(x-1)):(nEigen+nEigen*(x-1))]
yearavg <- function(x) mean(extract(x)) #their average
name <- function(x) CigCancer$STATE[1+nEigen*(x-1)] #name to display on graph
A <- matrix(nEigen*nCancer,nrow = nEigen, ncol = nCancer) #make an empty matrix
#Center and rescale the data, one column per station
for (i in 1:nCancer){
A[,i] <- (extract(i)-rep(yearavg(i),nEigen))/sqrt(nEigen)
}
#Make a symmetric square matrix
S <- A%*%t(A)

"Use eigen() to get the four eigenvalues and the change of basis matrix.
Then add the coeficients of the four eigenvectors as new columns in
your data frame."

Eig <- eigen(S); Eig
#            [,1]       [,2]        [,3] [,4]
# [1,]  0.8505223  0.1376321 -0.08757401  0.5
# [2,] -0.3094305 -0.3638980 -0.72237872  0.5
# [3,] -0.4018364  0.7546302  0.13806058  0.5
# [4,] -0.1392554 -0.5283643  0.67189215  0.5

"Which of the eigenvectors correlates most strongly with cigarette smok-
ing? Can you put an interpretation on the eigenvector for the largest
eigenvalue? The second largest?"
Eig$values #first eigenvalue is much larger than the others

"Make sure that you can reconstruct the data exactly by using all four
eigenvalues, then see how well you can reconstruct it using just two
principal components. It took me a while to get this right { see the
lecture notes or script 10D."

## USING FOUR EIGENVALUES
#The scale() function subtracts the column mean
#and divides each column by the corresponding scale factor
A.phi <- scale(A, center = TRUE, scale = rep(sqrt(nCancer-1),4));head(A.phi)
mean(A.phi[,1]); sum(A.phi[,1]^2); var(A[,1])
t(A.phi)%*%A.phi; S

#Now we have a symmetric matrix to which the spectral theorem applies.
#It has real positive eigenvalues and orthogonal eigenvectors
Eig <- eigen(S)
Eig$values  #first eigenvalue is much larger than the others
P <- Eig$vectors; P
PInv <- solve(P)
PInv%*%S%*%P    #diagonal matrix of eigenvalues

PInv <- solve(P)
PInv%*%S%*%P    #diagonal matrix of eigenvalues

#Now we can express each row of A.phi relative to the new basis of eigenvectors
A.eig <- A.phi%*%P

Cig.Eig<- data.frame(CigCancer$STATE, v1=A.eig[,1]);head(Cig.Eig)
Cig.Eig$v1
Cig.Eig[order(Cig.Eig$v1),]    #top states have least negative values

Cig.Eig<- data.frame(CigCancer$STATE, v1=A.eig[,1], v2=A.eig[,2])
Cig.Eig[order(Cig.Eig$v2),]
#Sort the original dataframe
CigCancer[order(Cig.Eig$v2),]

#Let's reconstruct the original data from all four eigenvectors
A2 <- cbind(Cig.Eig$v1, Cig.Eig$v2, A.eig[,3], A.eig[,4])
A.recon <- A2%*%PInv
head(A.recon);head(A.phi)
#Now we have to undo the scaling
A.c <- scale(A.recon,scale = (rep(1/sqrt(nCancer-1),4)))
#Finally we undo the centering.
AA <- scale(A.c, center = -colMeans(A), scale = FALSE)
head(AA)
head(A)

#Let's try to reconstruct the original data from just two eigenvectors
A2 <- cbind(Cig.Eig$v1,Cig.Eig$v2, 0,0)
A.recon <- A2%*%PInv
#Now we have to undo the scaling
A.c <- scale(A.recon,scale = c(rep(1/sqrt(nCancer-1),4)))
#Finally we undo the centering.
AA <- scale(A.c, center = -colMeans(A), scale = FALSE)
head(AA)
head(A)     #still fairly good