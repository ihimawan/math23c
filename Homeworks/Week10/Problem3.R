# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"From http://fairness-measures.org/Pages/Datasets I downloaded the Adult
Census data and kept just the numeric columns. The column \"RICH\" now
has the value 1 to indicate income greater than $50K."

"My modifed file, for use in this problem, is in census.csv on the Web site."

#install.packages("stats4")
Census <- read.csv("lib/census.csv"); head(Census)

"Use logistic regression, as in script 10D, to model the probability of being
rich as a function of age, as a function of the number of years of education,
and as a function of the reported capital gain. Plot the regression curve in
each case. Then see if you can create a linear combination of the columns
that you think is a better predictor. (The theory of how to do this goes
beyond the scope of this course.)"

#Convert the RICH column to a Bernoulli random variable
richs <- (as.numeric(Census$RICH==1)); head(richs)

# RICH as function of AGE
ages <- Census$AGE
plot(ages, richs)  # looks awful for a straight-line approximation
b <- cov(ages, richs)/var(ages) # obtain slope
# Find the intercept
a <- mean(richs) - b*mean(ages);a
#We can add this regression line to the plot of the data
abline(a, b, col = "red")

# we assume that p = exp(alpha x+beta)/(1 + exp(alpha x+beta))
#This function can never be less than zero nor greater than 1
#Start with minus the log of the likelihood function
MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*ages)/(1+exp(alpha+beta*ages)) )*richs +
          log(1/(1+exp(alpha+beta*ages)))*(1-richs) )
}
#R has a function that will maximize this function of alpha and beta
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
abline(h=0.5)
abline(v=c(30,40,60,80))
index <- which(ages == 30); head(index)
mean(richs[index])   #20%
index <- which(ages == 40)
mean(richs[index])   #33%
index <- which(ages == 50)
mean(richs[index])   #43%
index <- which(ages == 60)
mean(richs[index])   #32%
# Does not show good model for the probability of being rich as function of age

# RICH as function of YEARS OF EDUCATION
school <- Census$SCHOOL
plot(school, richs)
b <- cov(school, richs)/var(school) # obtain slope
# Find the intercept
a <- mean(richs) - b*mean(school);a
#We can add this regression line to the plot of the data
abline(a, b, col = "red")

MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*school)/(1+exp(alpha+beta*school)) )*richs +
          log(1/(1+exp(alpha+beta*school)))*(1-richs) )
}
#R has a function that will maximize this function of alpha and beta
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
abline(h=0.5)
abline(v=c(5,10,15))
# The curve shows a good model for the probability of being rich as function of schooling
index <- which(school == 5); head(index)
mean(richs[index])   #5%
index <- which(school == 10)
mean(richs[index])   #19%
index <- which(school == 15)
mean(richs[index])   #73%

# RICH as function of GAIN
gain <- Census$GAIN
plot(gain, richs)
b <- cov(gain, richs)/var(gain) # obtain slope
# Find the intercept
a <- mean(richs) - b*mean(gain);a
#We can add this regression line to the plot of the data
abline(a, b, col = "red")

MLL<- function(alpha, beta) {
  -sum( log( exp(alpha+beta*gain)/(1+exp(alpha+beta*gain)) )*richs +
          log(1/(1+exp(alpha+beta*gain)))*(1-richs) )
}

#R has a function that will maximize this function of alpha and beta
results<-mle(MLL, start = list(alpha = 0, beta = 0)) #an initial guess is required
results@coef
curve( exp(results@coef[1]+results@coef[2]*x)/ (1+exp(results@coef[1]+results@coef[2]*x)),col = "blue", add=TRUE)
abline(h=0.5)
abline(v=c(10^4,4*10^4,8*10^4))
# The curve shows a good model for the probability of being rich as function of schooling
index <- which(gain == 0); head(index)
mean(richs[index])   #20%
index <- which(gain > 0 & gain < 5000)
mean(richs[index])   #17%
index <- which(gain >= 5000 & gain < 10000)
mean(richs[index])   #84%
index <- which(gain >= 10000 & gain < 15000)
mean(richs[index])   #96%
index <- which(gain >= 15000 & gain < 20000)
mean(richs[index])   #100%
index <- which(gain >= 25000 & gain < 30000)
mean(richs[index])   #100%
index <- which(gain >= 35000 & gain < 40000)
mean(richs[index])   #NA
index <- which(gain >= 40000)
mean(richs[index])   #98%

# I would say that this is also a good predictor for being rich.

