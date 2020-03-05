# DATA WRANGLING PROBLEM 3
#
# Find on the Internet some data in which you can identify two groups, com-pare means or
# medians for some numeric quantity, and carry out a permu-tation test to assess whether
# or not the difference is statistically significant.Save  a  .cvs  file  that  includes
# just  the  data  that  you  are  analyzing.   Theprototype is the analysis of Beerwings
# .csv in script 2D.
#
# Extracted by myself from GSS: https://gssdataexplorer.norc.org/projects/73889/extracts/54955

# clean the data first

MarriageAge <- read.csv("lib/MarriageAge.csv", stringsAsFactors = FALSE); head(MarriageAge)
isNumeric <- function (x) {
  res <- x %in% regmatches(x, regexpr("\\d+", x))
}
MarriageAge <- MarriageAge[ , c("AgeWhenMarried", "DivorcedOrSeparated")] # Subset by name
MarriageAge <- subset(MarriageAge,
                       !is.na(MarriageAge$AgeWhenMarried) & isNumeric(MarriageAge$AgeWhenMarried)
                         & !is.na(MarriageAge$DivorcedOrSeparated)
                         & (MarriageAge$DivorcedOrSeparated == 'Yes' | MarriageAge$DivorcedOrSeparated == 'No'))
head(MarriageAge)
# obtained clean data!

# PART 1: Testing difference between number of marriage age between those who are divorced and not divorced
DivorcedAvgAge <- mean(as.numeric(MarriageAge$AgeWhenMarried[which(MarriageAge$DivorcedOrSeparated == "Yes")])) # 21.02222
NotDivorcedAvgAge <- mean(as.numeric(MarriageAge$AgeWhenMarried[which(MarriageAge$DivorcedOrSeparated == "No")])) # 24.4125
observed <- NotDivorcedAvgAge - DivorcedAvgAge; observed # Difference of 3.390278

# permutation test
N <- 10000
diffs <- numeric(N)
for (i in 1:N){
  DivorcedOrNot <- sample(MarriageAge$DivorcedOrSeparated)   # permuted divorce/separated or not column
  DivorcedAvgAge <- mean(as.numeric(MarriageAge$AgeWhenMarried[which(DivorcedOrNot == "Yes")]))
  NotDivorcedAvgAge <- mean(as.numeric(MarriageAge$AgeWhenMarried[which(DivorcedOrNot == "No")]))
  diffs[i] <- NotDivorcedAvgAge - DivorcedAvgAge
}

mean(diffs) # 0.006134674
hist(diffs, breaks = "FD", xlim = c(-4,4))
abline(v = observed, col = "red") # way off
pvalue <- (sum(diffs >= observed)+1)/(N+1); pvalue # around 9.999e-05
pvalue.2tailed <- pvalue*2; pvalue.2tailed # around 0.00019998
# the two tailed p-value says there is only 0.00019998 chance that the
# observed discrepancy is by chance. If we are using the significance
# level on .05, we have enough evidence against the null hypothesis that there
# is relationship between marriage age and whether or not you will divorce
