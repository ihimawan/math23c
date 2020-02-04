# setwd("Homeworks/Week1")
# cleanup
rm(list = ls())

# A) Generate a truth table with 16 lines for the logical expression (p ∧ q) =⇒ (r ∨ s)
# create the permutation table and assign the values
table <- expand.grid(p = c(TRUE, FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE), s = c(TRUE, FALSE))
p <- table$p
q <- table$q
r <- table$r
s <- table$s

# append function logic and print to console
table$f = (p & q) | !(r | s); table

# B) Use the truth table to write the expression in conjunctive normal form.
# the following is my answer to above
cnf = (p|!q|!r|!s)&(!p|q|!r|!s)&(p|q|!r|!s)&(p|!q|r|!s)&(!p|q|r|!s)&(p|q|r|!s)&(p|!q|!r|s)&(!p|q|!r|s)&(p|q|!r|s);

# C) Use R to prove that your answer is correct
table$cnf <- cnf; table
# indeed it is correct
#        p     q     r     s     f   cnf
# 1   TRUE  TRUE  TRUE  TRUE  TRUE  TRUE
# 2  FALSE  TRUE  TRUE  TRUE FALSE FALSE
# 3   TRUE FALSE  TRUE  TRUE FALSE FALSE
# 4  FALSE FALSE  TRUE  TRUE FALSE FALSE
# 5   TRUE  TRUE FALSE  TRUE  TRUE  TRUE
# 6  FALSE  TRUE FALSE  TRUE FALSE FALSE
# 7   TRUE FALSE FALSE  TRUE FALSE FALSE
# 8  FALSE FALSE FALSE  TRUE FALSE FALSE
# 9   TRUE  TRUE  TRUE FALSE  TRUE  TRUE
# 10 FALSE  TRUE  TRUE FALSE FALSE FALSE
# 11  TRUE FALSE  TRUE FALSE FALSE FALSE
# 12 FALSE FALSE  TRUE FALSE FALSE FALSE
# 13  TRUE  TRUE FALSE FALSE  TRUE  TRUE
# 14 FALSE  TRUE FALSE FALSE  TRUE  TRUE
# 15  TRUE FALSE FALSE FALSE  TRUE  TRUE
# 16 FALSE FALSE FALSE FALSE  TRUE  TRUE