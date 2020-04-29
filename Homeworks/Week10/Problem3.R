# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"From http://fairness-measures.org/Pages/Datasets I downloaded the Adult
Census data and kept just the numeric columns. The column \"RICH\" now
has the value 1 to indicate income greater than $50K."


"My modifed file, for use in this problem, is in census.csv on the Web site."

"Use logistic regression, as in script 10D, to model the probability of being
rich as a function of age, as a function of the number of years of education,
and as a function of the reported capital gain. Plot the regression curve in
each case. Then see if you can create a linear combination of the columns
that you think is a better predictor. (The theory of how to do this goes
beyond the scope of this course.)"

"I have also uploaded the original le to the Web site as BigCensus.csv.
This appears to be a very popular dataset for analysis. If you use the non-
numeric columns (race, sex, occupation, etc.) there is plenty of material for
a term project with an ethical angle to it."