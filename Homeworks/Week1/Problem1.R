grid4 <- expand.grid(c(T,F),c(T,F),c(T,F),c(T,F)); grid4
#grid4 <- expand.grid(p = c(TRUE,FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE), s = c(TRUE, FALSE))
p <- grid4$Var4;p
q <- grid4$Var3;q
r <- grid4$Var2;r
s <- grid4$Var1;s

lhs<-(p|q)&(r|s)
rhs<-(p&r)|(p&s)|(q&r)|(q&s)
identical(lhs,rhs)