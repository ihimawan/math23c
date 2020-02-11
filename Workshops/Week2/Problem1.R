# setwd("Workshops/Week2")
rm(list = ls()) # cleanup

multipliesOfPi <- floor((1:1001 * pi) * 1000) %% 1000
table(multipliesOfPi) # 17 is duplicated
which(multipliesOfPi == 17) # 99 212 325 438 551 664 777 890
piapprox <- ((-99+212) * pi)/(212-99); piapprox # 3.141593
