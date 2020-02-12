mod2 <- function(x) x %% 2
impl <- function(x, y) mod2(1 + x + y)
not <- function(x) mod2(x + 1)
or <- function(x, y) mod2(x + y + x*y)
and <- function(x, y) mod2(x*y)

# 2a
table1 <- expand.grid(p = c(TRUE,FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE))
p <- table1$p
q <- table1$q
r <- table1$r

table1$fx <- impl((impl(and(not(p), not(q)), r)), not(or(q, not(r))));
table1$dnf <- (p&q&r)|(!p&q&r)|(!p&!q&r)|(!p&!q&!r);
table1$cnf <- (!p|q|!r)&(!p|!q|r)&(p|!q|r)&(!p|q|r)

# 2b
table2 <- expand.grid(p = c(TRUE,FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE))
p <- table2$p
q <- table2$q
r <- table2$r

table2$fx <- or(and(p, q),and(not(q), r))
table2$dnf <- (p&q&r)|(p&!q&r)|(!p&!q&r)|(p&q&!r);
table2$cnf <- (p|!q|!r)&(p|!q|r)&(!p|q|r)&(p|q|r)

print("2a table result"); table1;
print("2b table result"); table2;
