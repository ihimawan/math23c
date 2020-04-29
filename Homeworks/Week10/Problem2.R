# setwd("Homeworks/Week9")
# cleanup
rm(list=ls())

"The file CigCancer.csv contains data about cigarette sales and the incidence
of various types of cancer in most of the states."

"Center and scale the cancer data by the procedure described in Hub-
bard. This is tricky { look at how it was done for the weather data in
the lecture notes."

"Use eigen() to get the four eigenvalues and the change of basis matrix.
Then add the coecients of the four eigenvectors as new columns in
your data frame."

"Which of the eigenvectors correlates most strongly with cigarette smok-
ing? Can you put an interpretation on the eigenvector for the largest
eigenvalue? The second largest?"

"Make sure that you can reconstruct the data exactly by using all four
eigenvalues, then see how well you can reconstruct it using just two
principal components. It took me a while to get this right { see the
lecture notes or script 10D."