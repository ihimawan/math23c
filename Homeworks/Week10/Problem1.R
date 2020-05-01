# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file QB1991.csv includes the quarterback salary and total team salary for every team in the NFL in 1991."
QB1991 <- read.csv("lib/QB1991.csv"); head(QB1991)
total <- QB1991$Total
qb <- QB1991$QB

"Make a scatter plot of total salary as a function of QB salary."
plot(total, qb, main="Scatterplot", xlab="Total ", ylab="QB", pch=19)

"Find the regression line by using the lm() function and add it to your
plot using abline()."

model <- lm(qb~total)
abline(model, col="red") # regression line (y~x)

"Use summary() on the object returned by lm() and determine which of
the reported R-squared values is equal to the square of the correlation."

summary(model)
#Call:
# lm(formula = QB1991$QB ~ QB1991$Total)
#
# Residuals:
#      Min       1Q   Median       3Q      Max
# -1007.05  -350.86   -74.57   304.43  1503.17
#
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)
# (Intercept)  -1.649e+03  8.524e+02  -1.935  0.06395 .
# QB1991$Total  1.363e-01  3.739e-02   3.647  0.00117 **
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Residual standard error: 575.8 on 26 degrees of freedom
# Multiple R-squared:  0.3384,	Adjusted R-squared:  0.3129
# F-statistic:  13.3 on 1 and 26 DF,  p-value: 0.001166

"Find the coeficients of the regression line by using the Gram-Schmidt
approach."



"Find the coeficients of the regression line by using the projection ma-
trix approach."

cor(qb,total)
A <- cbind(rep(1,nrow(QB1991)),total);head(A)
B <- t(A)%*%A; B
P <- A%*%solve(B)%*%t(A)
y.hat <- P%*%qb #predicted values, on the regression line
plot(total,qb,pch = ".",cex = 3) #scatter plot of the data
points(total,y.hat,type = "b") #and the regression line