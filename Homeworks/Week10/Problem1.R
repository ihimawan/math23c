# setwd("Homeworks/Week10")
# cleanup
rm(list=ls())

"The file QB1991.csv includes the quarterback salary and total team salary for every team in the NFL in 1991."
QB1991 <- read.csv("lib/QB1991.csv"); head(QB1991)
total <- QB1991$Total
qb <- QB1991$QB

"Make a scatter plot of total salary as a function of QB salary."
plot(qb, total, main="Scatterplot", xlab="QB SALARY ", ylab="TOTAL SLARY", pch=19)

"Find the regression line by using the lm() function and add it to your
plot using abline()."

model <- lm(total~qb)
abline(model, col="red") # regression line (y~x)

"Use summary() on the object returned by lm() and determine which of
the reported R-squared values is equal to the square of the correlation."
summary(model)
# Call:
# lm(formula = total ~ qb)
#
# Residuals:
#     Min      1Q  Median      3Q     Max
# -4030.2 -1910.4  -273.9  1672.8  4953.3
#
# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 1.905e+04  1.080e+03  17.633 5.53e-16 ***
# qb          2.482e+00  6.806e-01   3.647  0.00117 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 2457 on 26 degrees of freedom
# Multiple R-squared:  0.3384,	Adjusted R-squared:  0.3129
# F-statistic:  13.3 on 1 and 26 DF,  p-value: 0.001166

# intercept coefficients
a <- model$coefficients[1]; a # 19052.64
b <- model$coefficients[2]; b # 2.481745

#This means the prediction of the regression line is 19052.64  =  2.481745  * qb
abline(a, b, col = "red") # which is just the same as `abline(model, col="red")`

"Find the coeficients of the regression line by using the Gram-Schmidt
approach."

v1 <- rep(1, nrow(QB1991))
v2 <- qb - (sum(qb*v1)/sum(v1*v1))*v1

sum(v1*v2) # very small number close to zero - orthogonal!

# Finding coefficients:

# compute slope
GS.b <- sum(total*v2)/sum(v2*v2); GS.b

# Project onto the axis to get the predicted y-values.
PredY <- (sum(total*v1)/sum(v1*v1))*v1 + (sum(total*v2)/sum(v2*v2))*v2

# Recall that yhat = a + bx, so a = yhat - bx.
# Results in # vector of intercepts, which has same entry all of them
GS.a <- PredY - GS.b*qb; head(GS.a)
# [1] 19052.64 19052.64 19052.64 19052.64 19052.64 19052.64

# extract as an object our intercept.
GS.a <- GS.a[1]; GS.a

# Finally, let's check that our coefficietns match.
model$coefficients[1]; GS.a # 19052.64; it does!
model$coefficients[2]; GS.b # 2.481745; it does!

"Find the coeficients of the regression line by using the projection ma-
trix approach."

cor(total,qb)
A <- cbind(rep(1,nrow(QB1991)),qb);head(A)
B <- t(A)%*%A; B
P <- A%*%solve(B)%*%t(A)
y.hat <- P%*%total #predicted values, on the regression line
plot(qb,total,pch = ".",cex = 3) #scatter plot of the data
points(qb,y.hat,type = "b") #and the regression line