## Ce fichier contient l'analyse univariée des données pour les variables:
## âge du conducteur (age_conduct), type du véhicule (type_veh) et bonus malus
## (bonus_malus).
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


### Données sur l'âge des conducteurs (age_conduct)
## Enlever les NA
age_conduct1 <- na.omit(MontantReclFr$age_conduct)
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
hist(age_conduct)
## Nuage de points
plot(MontantReclFr$mont_recl~MontantReclFr$age_conduct,
     main = "Montant de réclamation en fonction de l'âge du conducteur",
     xlab = "Âge conducteur", ylab = "Montant réclamation", ylim = c(0, 10000))


### Données sur le type de véhicule (type_veh)
## Catégories de la liste :
unique(MontantReclFr$type_veh)
## Nombre de véhicules de chaque type.
table(MontantReclFr$type_veh)


### Données sur bonus malus (bonus_malus)
## Catégories de la liste :
unique(MontantReclFr$bonus_malus)
## Nombre de bonus pour chaque score
table(MontantReclFr$bonus_malus)
## Moyenne
mean(MontantReclFr$bonus_malus)
## Écart-type
sd(MontantReclFr$bonus_malus)
## Médiane
median(MontantReclFr$bonus_malus)
