# setwd("Homeworks/Week2")
rm(list = ls()) # cleanup

# A) Check answer with Question #4
m <- 6 # number of trumps
n <- 20 # number of non-trumps
h <- 13 # number on hand
dhyper(0:3, m, n, h) # [1] 0.007453416 0.072670807 0.242236025 0.355279503

# B) Estimate probability for East to have 3 trumps by using permutation test
k <- 3 # having 3 trumps on hand
Direction <- c("EAST", "WEST")
positions <- combn(1:26, 6); positions
pos1 <- positions[1, ];
pos2 <- positions[2, ];
df <- data.frame(pos1, pos2); head(df)



