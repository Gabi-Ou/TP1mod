## Travail Pratique en Modèles linéaires

x <- read.csv('MontantReclFr.csv')
attach(x)

is.na(x$mont_recl) <- -1

x$mont_recl[is.na(x$mont_recl)] <- -1

genre[genre == 'femme'] <- 0
genre[genre == 'homme'] <- 1

library(ggplot2)

boxplot(mont_recl~genre, data= x)
