rm(list = ls())

#A
gsl <- read.csv("lib/GSSLogical.csv")
attach(gsl)
answerA <- length(which(!Male & !Republican & GunOwner)); answerA
detach(gsl)

#B
rsx <-  read.csv("lib/RedSox2013.csv")
attach(rsx)
# I don't know much about baseball, so I assume the `R` column includes the runs of two teams together
answerB <- which.min(Duration[which(R >= 10)]); answerB
detach(rsx)

#C
boxplot(R~DayNight, data=rsx)

#D
attach(rsx)
pWonLost <- mean(WonLost == 'L')
pAway <- mean(!Away)
expected <- nrow(rsx)*outer(c(pWonLost, 1-pWonLost), c(pAway, 1-pAway)); expected
observed <- table(WonLost, Away); observed
chisq <- sum((observed-expected)^2/expected); chisq
pValue <- 1 - pchisq(chisq, 1); pValue
chisq.test(WonLost, Away, correct=FALSE)
detach(rsx)
