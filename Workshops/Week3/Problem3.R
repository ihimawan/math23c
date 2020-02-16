setwd("Workshops/Week3")
HK <- read.csv("lib/HorseKicks.csv")

# A) see if consistent with Poisson
# combine to single vector
combine <- c(HK$GC, HK$C1, HK$C2, HK$C3, HK$C4, HK$C5, HK$C6, HK$C7, HK$C8, HK$C9, HK$C10, HK$C11, HK$C14, HK$C15)
lambda <- mean(combine); lambda

combineTable<-table(combine); combineTable
#Should not do chi square with tiny counts.  Combine all games with 3 or more kicks
#Make the vector have only 4 elements
combineTable[4]<-sum(combineTable[4:5]);combineTable <- combineTable[-5]; combineTable #(top row)

#assuming the data are Poisson with the calculated lambda = 0.7
Expected<-162*dpois(0:2, lambda);Expected[4]<-162*(1-ppois(3, lambda))
Expected #(bottom row)
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}
ChiPhil<-ChiSq(combineTable,Expected);ChiPhil #  2356.189
#How probable is this large a value, given the chi-square distribution?
#Because we estimated one parameter from the data and used the total home runs,
#there are only three degrees of freedom
#First approach: use our ChiSq function and the built-in pchisq function
Pvalue<- pchisq(ChiPhil,2,lower.tail = FALSE); Pvalue # 0
#Conclusion -- the observed data are consistent with a Poisson distribution

# B)
tbl <- tail (sapply(HK, sum), -1)
chisq.test(tbl)
#
# 	Chi-squared test for given probabilities
#
# data:  tbl
# X-squared = 27.286, df = 13, p-value = 0.01137