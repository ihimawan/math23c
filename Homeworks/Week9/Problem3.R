# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"As a function of time, daily temperature is a fairly smooth function, and
using 100 Fourier coecients provided a good reconstruction for Boston.
Daily precipitation is not a smooth function of time. However, Califor-
nia has a rainy season and a dry season, and the annual cycle should be
revealed by Fourier analysis. Analyze the precipitation data in the le
LAWeather.csv using the same approach that was used for Boston temper-
ature in workshop problem 2. Introduce a variable ncoeff for the number
of Fourier coecients. Draw graphs in R to show the following:
 ncoe =10 discovers the annual cycle.
 ncoe = 100 reveals that some years are rainier than others.
 ncoe = 1000 does a very good reconstruction."