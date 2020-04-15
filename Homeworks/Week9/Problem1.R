# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"Use  Gram-Schmidt  to  construct  a  “new”  orthonormal  basis  for V,  and graph the four new basis vectors on a
single set of axes.  Feel free to reusecode from script 9M, topic 4"

#install.packages("pracma")
library(pracma)

#We use Gram-Schmidt to create an orthonormal basis for polynomials
#of degree 3 or less on the interval [0, Inf].
#A polynomial is represented by a vector of four coefficients:
x0 <- c(1,0,0,0)
x1 <- c(0,1,0,0)
x2 <- c(0,0,1,0)
x3 <- c(0,0,0,1)

#evaluate a polynomial function, given its vector of coefficients.
f0 <- function(x) exp(-x)
f1 <- function(x) x * exp(-x)
f2 <- function(x) x^2 * exp(-x)
f3 <- function(x) x^3 * exp(-x)

polyEval <- function (coeff, x) {
  sum(coeff*c(f0(x), f1(x), f2(x), f3(x)))
}

# multiply the functions then integrate the product from 0 to Inf.
inner <- function(v,w) {
  fprod <- function(x) polyEval(v,x)*polyEval(w,x)
  integrate(Vectorize(fprod), 0,Inf )$value
}

inner(x0,x0); inner(x1,x1); inner(x2,x2)   #looks correct

#First do the even-degree polynomials.
v0 <- x0/sqrt(inner(x0,x0)); v0   #vector of length 1
sqrt(inner(v0, v0)) # 1 - means it is normalized

#find the next v1
u1 <- x1 - inner(x1,v0)*v0; u1 # unnormalized
v1 <- u1/sqrt(inner(u1,u1)); v1
sqrt(inner(v1, v1)) # 1 - means it is normalized

# check if v0 and v1 are orthogonal
inner(v0,v1) # basically zero - orthogonal!

#find v2
u2 <- x2 - inner(x2,v0)*v0 - inner(x2,v1)*v1; u2
v2 <- u2/sqrt(inner(u2,u2)); v2   #vector of length 1
sqrt(inner(v2, v2)) # 1 - means it is normalized

# check if v2 are orthogonal to v0 and v1
inner(v0,v2) # basically zero - orthogonal!
inner(v1,v2) # basically zero - orthogonal!

# find v3
u3 <- x3 - inner(x3,v2)*v2 - inner(x3,v1)*v1 - inner(x3,v0)*v0; u3
v3 <- u3/sqrt(inner(u3,u3)); v3   #vector of length 1
sqrt(inner(v3, v3)) # 1 - means it is normalized

# check if v3 are orthogonal to v0 and v1 and v2
inner(v3,v0) # basically zero - orthogonal!
inner(v3,v1) # basically zero - orthogonal!
inner(v3,v2) # basically zero - orthogonal!

# check all three are basis..
# which means they're lineraly independent vectors
# row reducie is a way to find out. but we have a package.
A <- cbind(v0,v1,v2,v3); A
rref(A)
#      v0 v1 v2 v3
# [1,]  1  0  0  0
# [2,]  0  1  0  0
# [3,]  0  0  1  0
# [4,]  0  0  0  1

# Since we get identity, they are lineraly independent. Hence, they are basis.

"graph the four new basis vectors on a single set of axes"

v0; v1; v2; v3 # these are concrete representation of ORTHONORMAL basis.

#Now, write functions for ABSTRACT basis functions
a0 <- function (x) { v0[1] * f0(x) + v0[2] * f1(x) + v0[3] * f2(x) + v0[4] * f3(x) }
a1 <- function (x) { v1[1] * f0(x) + v1[2] * f1(x) + v1[3] * f2(x) + v1[4] * f3(x) }
a2 <- function (x) { v2[1] * f0(x) + v2[2] * f1(x) + v2[3] * f2(x) + v2[4] * f3(x) }
a3 <- function (x) { v3[1] * f0(x) + v3[2] * f1(x) + v3[3] * f2(x) + v3[4] * f3(x) }

curve(a0, from=-1, to=1, col="red")
curve(a1, from=-1, to=1, col="green", add = TRUE)
curve(a2, from=-1, to=1, col="blue", add = TRUE)
curve(a3, from=-1, to=1, col="pink", add = TRUE)