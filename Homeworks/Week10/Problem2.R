# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file CigCancer.csv contains data about cigarette sales and the incidence
of various types of cancer in most of the states."
CigCancer <- read.csv("lib/CigCancer.csv"); head(CigCancer)

"Center and scale the cancer data by the procedure described in Hub-
bard. This is tricky { look at how it was done for the weather data in
the lecture notes."

nCancer <- nrow(CigCancer)/12;nCancer

extract <- function(x) CigCancer$CIG[(1+12*(x-1)):(12+12*(x-1))] #12 temperatures
yearavg <- function(x) mean(extract(x)) #their average
name <- function(x) CigCancer$STATE[1+12*(x-1)] #name to display on graph
A <- matrix(12*nCancer,nrow = 12, ncol = nCancer) #make an empty matrix
#Center and rescale the data, one column per station
for (i in 1:nCancer){
A[,i] <- (extract(i)-rep(yearavg(i),12))/sqrt(12)
}
#Make a symmetric square matrix
S <- A%*%t(A)

"Use eigen() to get the four eigenvalues and the change of basis matrix.
Then add the coeficients of the four eigenvectors as new columns in
your data frame."

Eig <- eigen(S)

"Which of the eigenvectors correlates most strongly with cigarette smok-
ing? Can you put an interpretation on the eigenvector for the largest
eigenvalue? The second largest?"
Eig$values #first eigenvalue is much larger than the others

"Make sure that you can reconstruct the data exactly by using all four
eigenvalues, then see how well you can reconstruct it using just two
principal components. It took me a while to get this right { see the
lecture notes or script 10D."

#The scale() function subtracts the column mean
#and divides each column by the corresponding scale factor
A.phi <- scale(A, center = TRUE, scale = c(rep(sqrt(nCancer-1),4)));head(A.phi)
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

RUN.Eig<- data.frame(RUN$Country,v1=A.eig[,1]);head(RUN.Eig)
RUN.Eig$v1
RUN.Eig[order(RUN.Eig$v1),]    #top countries have least negative values

RUN.Eig<- data.frame(RUN$Country,v1=A.eig[,1],v2=A.eig[,2])
RUN.Eig[order(RUN.Eig$v2),]    #top countries have least negative values
#Sort the original dataframe
RUN[order(RUN.Eig$v2),]    #perhaps sprinters at the top of the list

#Let's reconstruct the original data from all four eigenvectors
A2 <- cbind(RUN.Eig$v1,RUN.Eig$v2, A.eig[,3],A.eig[,4])
A.recon <- A2%*%PInv
head(A.recon);head(A.phi)
#Now we have to undo the scaling
A.c <- scale(A.recon,scale = c(rep(1/sqrt(m-1),4)))
#Finally we undo the centering.
AA <- scale(A.c, center = -colMeans(A), scale = FALSE)
head(AA)
head(A)     #perfect reconstruction