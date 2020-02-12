setwd("/Users/IndriHimawan/Desktop/MATH23C/Week\ 1");

# problem 1
table1 <- expand.grid(p = c(TRUE,FALSE), q = c(TRUE, FALSE), r = c(TRUE, FALSE), s = c(TRUE, FALSE))
p <- table1$p
q <- table1$q
r <- table1$r
s <- table1$s

lhs <- (p|q)&(r|s)
rhs <- (p&r)|(p&s)|(q&r)|(q&s)
identical(lhs, rhs);

# problem 2
table2 <- expand.grid(p = c(1,0), q = c(1, 0), r = c(1, 0))
p <- table2$p
q <- table2$q
r <- table2$r
fx <- (1+p+q*r+p*q*r) %% 2
table2$fx <- (1+p+q*r+p*q*r) %% 2
table2$dnf <- (!p&!q&r)|(!p&q&!r)|(!p&!q&!r)
table2$cnf <- (!p|!q|!r)&(p|!q|!r)&(!p|q|!r)&(!p|!q|r)&(!p|q|r)
table2;

#problem 3
redsox2013 <- read.csv("RedSox2013.csv"); 
attach(redsox2013)

# a) bar plot
score.diff <-  R - RA; score.diff
barplot(score.diff)

# b) max attendance
max(redsox2013$Attendance[which(DayNight == 'N' & WonLost == 'L')])

detach(redsox2013)

# problem 4

# a)
boxplot(hp~cyl, data=mtcars)

# b)
plot(mtcars$mpg, mtcars$am) # seems like there isn't any relationship.
# maybe try to do line regression or something

# c)
plot(mtcars$hp, mtcars$disp)

# d)
plot(mtcars$disp, mtcars$hp)








