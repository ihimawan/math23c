# Taken from Data Wranging Problems number 1
#
# Download  from  the  Internet  the  General  Social  Survey  (GSS)  for  a  year other  than  2002.
# Extract  from  it  a  dataframe  with  four  to  six  factor  or logical columns and save it to a .csv file.
# Then,  as in script 1D, topic 1,find or count rows with specified features, and form and apply a
# chi-squaretest to a couple of 2 times 2 contingency tables.
#
# Extracted by myself from GSS: https://gssdataexplorer.norc.org/projects/73889/extracts/53449

# setwd("Homeworks/DataWrangling")
gss <- read.csv("lib/GSSDataWrangling.csv")
attach(gss)
# WRKSLF    = R self-emp (1) or works for somebody (2)
# DIVORCE   = Ever been divorced or separated(1) or not(2)
# SPWRKSLF  = Spouse self-emp.(1) or works for somebody(2)
# PAWRKSLF  = Father self-emp.(1) or worked for somebody(2)
# MAWRKSLF  = Mother self-emp.(1) or worked for somebody(2)

# clean up table of "don't know" "no answer" "not applicable" responses
isValid <- function (val) val == 1 | val == 2
index <- which(isValid(WRKSLF) & isValid(DIVORCE) & isValid(SPWRKSLF) & isValid(PAWRKSLF) & isValid(MAWRKSLF))
gssCleaned <- gss[index,]

detach(gss)
attach(gssCleaned)

# 1) want to know if there is a correlation of being self employed and the spouse being self employed
#
# build contigency table
pWRKSLF <- mean(WRKSLF == '1')
pSPWRKSLF <- mean(SPWRKSLF == '1')
expected <- nrow(gssCleaned) * outer(c(pWRKSLF, 1 - pWRKSLF), c(pSPWRKSLF, 1 - pSPWRKSLF)); expected
#          [,1]      [,2]
# [1,]  9.42145  67.57855
# [2,] 71.57855 513.42145

chisq.test(WRKSLF, SPWRKSLF, correct=FALSE)
#
# 	Pearson's Chi-squared test
#
# data:  WRKSLF and SPWRKSLF
# X-squared = 12.556, df = 1, p-value = 0.0003948
#
# Using 0.05 significance level, Since p-value = 0.0003948 is less than 0.05 significance level, we reject the
# null hypothesis that self and spouse being self employed is independent

# 2) want to know if there is a correlation of being self employed and one of the parents being self-employed
#
# build 2x2 contigency table
PMAWRKSLF <- PAWRKSLF == '1' | MAWRKSLF == '1'
pPMAWRKSLF <- mean(PMAWRKSLF)
expected <- nrow(gssCleaned) * outer(c(pWRKSLF, 1 - pWRKSLF), c(pPMAWRKSLF, 1 - pPMAWRKSLF)); expected
#           [,1]      [,2]
# [1,]  20.12236  56.87764
# [2,] 152.87764 432.12236

chisq.test(WRKSLF, PMAWRKSLF, correct=FALSE)
#
#
# 	Pearson's Chi-squared test
#
# data:  WRKSLF and PMAWRKSLF
# X-squared = 6.0002, df = 1, p-value = 0.0143
#
# Using 0.05 significance level, Since p-value = 0.0143 is less than 0.05 significance level, we reject the
# null hypothesis that self and one of parent being self employed is independent

detach(gssCleaned)
