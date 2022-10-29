## Ce fichier contient l'analyse univariée des données pour les variables:
## âge du conducteur (age_conduct), type du véhicule (type_veh) et bonus malus
## (bonus_malus).
library(ggplot2)
MontantReclFr <- read.csv("MontantReclFr.csv")
View(MontantReclFr)
## La variable endogène dépendante (Y) est :
## le montant des réclamations (mont_recl).


### Données sur les montants de réclamation (mont_recl):
## Moyenne
mean(MontantReclFr$mont_recl)
## Écart-type
sd(MontantReclFr$mont_recl)
## Médiane
median(MontantReclFr$mont_recl)
## Extremums
min(MontantReclFr$mont_recl)
max(MontantReclFr$mont_recl)
##


### Données sur l'âge des conducteurs (age_conduct)
## Enlever les NA
age_conduct1 <- na.omit(MontantReclFr$age_conduct)
mont_recl1 <- MontantReclFr$mont_recl[age_conduct1]
## Moyenne
mean(age_conduct1)
## Écart-type
sd(age_conduct1)
## Médiane
median(age_conduct1)
## Extremums
min(age_conduct1)
max(age_conduct1)

### Histogramme
hist(age_conduct1, main = "Âge des conducteurs", xlab = "Âge des conducteurs",
     ylab = "Fréquence")
## Nuage de points
plot(mont_recl1~age_conduct1,
     main = "Montant de réclamation en fonction de l'âge du conducteur",
     xlab = "Âge conducteur", ylab = "Montant réclamation", ylim = c(0, 10000),
     cex = 0.2)

ggplot(data = MontantReclFr, aes(y = mont_recl, x = age_conduct)) + geom_point()


### Données sur le type de véhicule (type_veh)
## Catégories de la liste :
unique(MontantReclFr$type_veh)
## Nombre de véhicules de chaque type.
table(MontantReclFr$type_veh)
boxplot(mont_recl~type_veh)
ggplot(data = MontantReclFr, aes(y = mont_recl, x = type_veh)) + geom_point()


### Données sur bonus malus (bonus_malus)

## Retirer les 2 valeurs de -5, qui sont des erreurs car on veut [50; 350[
bonus_malus1 <- MontantReclFr$bonus_malus[MontantReclFr$bonus_malus > -5]
### Nouvelle variable pour les montants réclamations correspondants:
mont_recl1 <- MontantReclFr$mont_recl[MontantReclFr$bonus_malus > -5]

## Catégories de la liste :
unique(bonus_malus1)
## Nombre de bonus pour chaque score
table(bonus_malus1)
## Moyenne
mean(bonus_malus1)
## Écart-type
sd(bonus_malus1)
## Médiane
median(bonus_malus1)

boxplot(mont_recl1~bonus_malus1)
plot(mont_recl1~bonus_malus1,
     main = "Montant de réclamation en fonction du bonus",
     xlab = "Bonus", ylab = "Montant réclamation", ylim = c(0, 10000),
     cex = 0.2)