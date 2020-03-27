# setwd("Homeworks/Week7")
# cleanup
rm(list=ls())

# These replace the R take-home test that was advertised in the syllabus.
#
# 1.  Go  to  the  Internet  and  find  one  large  or  two  small  datasets  related
# to respiratory viral infectons, either Covid-19 or something historical like the
# “Spanish  flu”  of  1918  or  the  “swine  flu”  of  2009.   They  just  have
# to  besufficient for doing the problems that follow.  Convert the data to a .csv file
# that can be used by other members of the class.
#
# Data source: Novel Corona Virus 2019 Dataset
# Day level information on covid-19 affected caseshappyScorewww.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset
COVID <- read.csv("lib/COVID19_line_list_data.csv");

# 2.  Using your datasets, make a boxplot show interesting relationships.
#
# Looking at the data just from sight, I want to see any correlation between if they likelyhood of death/recovery
# had to do with their ages.

# first, get a cleaner version of the data for the boxplot by getting rid of all the NA
# and only getting patients in a terminal state (either in death or recovered)
COVID.recovery <- subset(COVID, age != "NA" & (death != 0 | recovered != 0), select=c(age, death, recovered));
COVID.recovery$result <- ifelse(COVID.recovery$death == 0, "RECOVERED", "DIED") # 1 would be recovered and 0 would be death
boxplot(age~result, data=COVID.recovery, main="Age vs Death/Recovery",
        xlab="Recovered", ylab="Age")
# we can see that those who recovered are visibly younger than those who died.

# further shown as such from these manually calculated means
RecoveredAvgAge <- mean(COVID.recovery$age[which(COVID.recovery$result == "RECOVERED")]) # 42.21207
DiedAvgAge <- mean(COVID.recovery$age[which(COVID.recovery$result == "DIED")]) # 68.58621
observed <- DiedAvgAge - RecoveredAvgAge # 26.37414 # The difference is up to 26 years!

# 2. Using your datasets, make a scatter plot that show interesting relationships.
#
# Looking at the data, I want to see if the more COVID cases as the date increases
# first clean up the data, because the date formatting is not uniform
COVID.confirmed <- subset(COVID, reporting.date != "NA", select=reporting.date)
safe.ifelse <- function(cond, yes, no){ class.y <- class(yes)
                                  X <- ifelse(cond, yes, no)
                                  class(X) <- class.y; return(X)}
COVID.confirmed$formatted.date <- safe.ifelse(nchar(as.character(COVID.confirmed$reporting.date)) == 8,
                                    as.Date(as.character(COVID.confirmed$reporting.date), "%m/%d/%y", tz="GMT"),
                                    as.Date(as.character(COVID.confirmed$reporting.date), "%m/%d/%Y", tz="GMT"))
frequency <- table(COVID.confirmed$formatted.date)
frequency.df <- as.data.frame(frequency)
plot(frequency.df$Var1, frequency.df$Freq, xlab = "date", ylab = "confirmed cases", main = "Confirmed cases by date")
# seems like there is some correlation, but we can notice that there is a spike of confirmed cases at the end of February, instead of a steady increase.
# there is also a peak mid-January, maybe that's when people started to realize to report given COVID-19 symptoms.

# 3.  Formulate an interesting hypothesis and test it by using a permutation test.
#
# I want to test that age has a lot to do whether or not you will recover
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  DiedOrRecovered <- sample(COVID.recovery$result) # permuted died/recovered column
  RecoveredAvgAge <- mean(COVID.recovery$age[which(DiedOrRecovered == "RECOVERED")])
  DiedAvgAge <- mean(COVID.recovery$age[which(DiedOrRecovered == "DIED")])
  diffs[i] <- DiedAvgAge - RecoveredAvgAge
}

mean(diffs) # 0.03646172
hist(diffs, breaks = "FD", xlim = c(-15,27)) # looking good
abline(v = observed, col = "red") # way on the right
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 9.999e-05
# the p-value says there is only 9.999e-05 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence to reject the null hypothesis that there
# is no difference in mean age for recovery/death and toward the alternative hypothesis
# that there is a difference. Shown by how people who are younger have a better chance
# of recovery.

# 4.  Find some aspect of your datasets than can be modeled by a discrete dis-tribution,
# and support your model with histograms and a chi-square test.
#
# Looking at the number of COVID cases per day, it doesn't look like any particular discrete distribution.
# Maybe a Poisson distribution?

# we can just use the same `frequency` from the scatterplot
lambda <- mean(frequency); lambda # if this were a poisson distribution, the mean would be 25.2093
hist(frequency, breaks = "FD")  # looks reasonably like a poisson distribution!
# With that, let's use the following as my hypothesis.
# (*)Hypothesis: The number of confirmed patients overtime each day is modeled by a Poisson distribution

# let's see if we need to clump together some values
breaks <- hist(frequency, breaks = "FD")$breaks
#  [1]   0  10  20  30  40  50  60  70  80  90 100
observed <- hist(frequency, breaks = "FD")$count
#  [1]  8 12 12  3  3  2  1  1  0  1
# let's clump 30<x<=50 and 50<x
observed[4] <- sum(observed[4:5])
observed[5] <- sum(observed[6:10])
observed <- observed[-(6:10)]
# [1]  8 12 12  6  5
# looking good!

N <- length(frequency); N # 43
expected <- N * c(sum(dpois(0:10, lambda)),
              sum(dpois(11:20, lambda)),
              sum(dpois(21:30, lambda)),
              sum(dpois(31:50, lambda)),
              ppois(50, lambda, lower.tail = FALSE))

# sanity check
sum(expected) # indeed equals to N = 43. Yay!

# put barplot side by side
barplot(rbind(observed, expected), beside = TRUE, col = c("red", "blue"))
# I can definitely see slight correlation, but the observed is less extreme compared to what's expected.

# use chisquared test
ChiSq <-function(Obs,Exp){
  sum((Obs-Exp)^2/Exp)
}

n <- length(observed)
# degrees of freedom is n-2 since we expected total equal to the actual total
# and computed lambda from data
chisq <- sum((observed-expected)^2/expected); chisq # 141660.6
pValue <- pchisq(chisq, df = n-2, lower.tail = FALSE); pValue # 0
# this means that there is 0% chance that this result happens as a
# poisson distribution. With this value, we reject
# null hypothesis that our data did come from a poisson distribution.

# 5.  Find some aspect of your datasets than can be modeled by a continuous distribution,
# and support your model with a histogram overlaid with a curve plus a chi-square test.
#
# From the fact that there are overwhelmingly more younger people recovering compared to the elder people,
# maybe the age and whether or not you will recover based on your age is based on a continuous distribution.
# Let's create a histogram out of those who recovered.

# reuse the same data from the boxplot
recoveredAges <- COVID.recovery$age[which(COVID.recovery$result == "RECOVERED")] # 1 would be recovered and 0 would be death
nRecovered <- length(recoveredAges) # 145
hist(recoveredAges, probability = TRUE) # how interesting! It looks like it's a regular normal distribution. Now lets try to overlay it with the normal distribution graph
mu <- mean(recoveredAges); mu # 42.21207
sigma <- sd(recoveredAges); sigma # 17.72647
curve(dnorm(x, mu, sigma), add = TRUE, col = "red")  # it matches well! Let's use that as my hypothesis

# (*)Hypothesis: recovery age is modeled by a normal distribution.
# I will start by Comparing with the  normal  distribution by  using  deciles.

# Now make a vector of deciles
dec <- qnorm(seq(0.0, 1, by = 0.1), mu, sigma); dec   #11 bins
Exp <- rep(nRecovered/10,10); Exp     # expected ages per bin

binAges <- numeric(10)
for (i in 1:10)
  binAges[i] <- sum((recoveredAges >= dec[i]) & (recoveredAges <= dec[i+1]) )

binAges
# [1] 13  9 20 17 16 10 12 23  9 16

chisq <- sum((binAges - Exp)^2/Exp); chisq # 13.96552
#We estimated two parameters, which costs two degrees of freedom
pValue <- pchisq(chisq, df = 7, lower.tail = FALSE); pValue # 0.05179807
# Using 0.05 significance level, Since p-value 0.05179807 is (only slightly) greater than 0.05 significance level,
# we do not reject the null hypothesis that the normal distribution was a good model for the dataset. (yay!)

