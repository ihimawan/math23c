rm(list = ls())
source("lib/operations.R")

# A
table <- expand.grid(p = c(TRUE, FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE), s = c(TRUE, FALSE))
p <- table$p
q <- table$q
r <- table$r
s <- table$s

table$f = (p & q) | !(r | s); table

#B
cnf = (p|!q|!r|!s)&(!p|q|!r|!s)&(p|q|!r|!s)&(p|!q|r|!s)&(!p|q|r|!s)&(p|q|r|!s)&(p|!q|!r|s)&(!p|q|!r|s)&(p|q|!r|s);

#C
table$cnf <- cnf; table