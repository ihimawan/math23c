# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"Use  Gram-Schmidt  to  construct  a  “new”  orthonormal  basis  for V,  and graph the four new basis vectors on a
single set of axes.  Feel free to reusecode from script 9M, topic 4"

#We use Gram-Schmidt to create an orthonormal basis for polynomials
#of degree 3 or less on the interval [0, Inf].
#A polynomial is represented by a vector of four coefficients:
x0 <- c(1,0,0,0)
x1 <- c(0,1,0,0)
x2 <- c(0,0,1,0)
x3 <- c(0,0,0,1)

#evaluate a polynomial function, given its vector of coefficients.
polyEval <- function (coeff,x) {
  sum(coeff*c(exp(-x), x*exp(-x), x^2*exp(-x), x^3*exp(-x)))
}

# multiply the functions then integrate the product from 0 to Inf.
inner <- function(v,w) {
  fprod <- function(x) polyEval(v,x)*polyEval(w,x)
  integrate(Vectorize(fprod), 0,Inf )$value
}

inner(x0,x0); inner(x1,x1); inner(x2,x2)   #looks correct

#First do the even-degree polynomials.
v0 <- x0/sqrt(inner(x0,x0)); v0   #vector of length 1
#Use Gram-Schmidt to make an orthonormal basis.
y2 <- x2 - inner(x2,v0)*v0;
v2 <- y2/sqrt(inner(y2,y2)); v2   #vector of length 1

#Now do the odd-degree polynomials.
v1 <- x1/sqrt(inner(x1,x1)); v1   #vector of length 1
#Use Gram-Schmidt to make an orthonormal basis vector
y3 <- x3 - inner(x3,v1)*v1; y3
v3 <- y3/sqrt(inner(y3,y3)); v3   #vector of length 1

#Success: we now have 4 orthogonal polynomials, all of length 1
inner(v0,v0); inner(v1,v1); inner(v2,v2); inner(v3,v3)    #length is 1
inner(v0,v1); inner(v1,v3)   # TODO NOT DOIN IT #orthogonal since multiplication is zero (or very close to zero)
