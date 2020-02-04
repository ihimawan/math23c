#Taken from Data Wranging Problems number 1
#
# Download  from  the  Internet  the  General  Social  Survey  (GSS)  for  a  year other  than  2002.
# Extract  from  it  a  dataframe  with  four  to  six  factor  or logical columns and save it to a .csv file.
# Then,  as in script 1D, topic 1,find or count rows with specified features, and form and apply a
# chi-squaretest to a couple of 2 times 2 contingency tables.
#
# Extracted by myself on GSS: https://gssdataexplorer.norc.org/projects/73889/extracts/53449

# setwd("Homeworks/Week1")
gss <- read.csv("lib/GSSDataWrangling.csv")
# WRKSLF    = R self-emp or works for somebody
# DIVORCE   = Ever been divorced or separated
# SPWRKSLF  = Spouse self-emp. or works for somebody
# PAWRKSLF  = Father self-emp. or worked for somebody
# MAWRKSLF  = Mother self-emp. or worked for somebody
