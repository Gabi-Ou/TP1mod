### ACT-2003 Modèles linéaires en actuariat
## Travail pratique 1
##
## Étape 1 : Importation des donnnées

donnees <- read.csv('MontantReclFr.csv')
attach(donnees)

## Étape 2 : Analyse univariée des données

## Variables analysées par Émilie :
## 1- garage : Type de garage habituel du réclamant
## 2- source_energ : Source d'énergie qui permet de faire
##                   fonctionner le véhicule accidenté
## 3- valeur_veh : Valeur du véhicule accidenté en dollars. On
##                 ne spécifie pas qu'il s'agit de la valeur 
##                 à l'achat ou actuelle.

## Terminons avec la troisième variable, valeur_veh.

## First tought : À la première lecture de cette variable, je crois
##                que cette varable impacte les montants réclamés puisque
##                la sévérité va être touché.
##
## On s'intéresse donc ici le montant des réclamations en fonction de la valeur
## du véhicule de l'assuré.

## Variable exogène : Valeur du véhicule
## Variable endogène : Montant des réclamations

## Nuage de points
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation")

## Je vais mettre des limites pour essayer de mieux voir ce qui se passe
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation", ylim = c(0,50000))
abline(modele,col = 'blue')
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation", ylim = c(0,40000))
abline(modele,col = 'blue')
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation", ylim = c(0,30000))
abline(modele,col = 'blue')
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation", ylim = c(0,10000))
abline(modele,col = 'blue')
plot(mont_recl ~ valeur_veh, main = "Montant des réclamations en fonction de la
     valeur du véhicule", xlab = "Valeur du véhicule", ylab = "Montant des réclamation", ylim = c(0,5000))
abline(modele,col = 'blue')

## Commentaires sur le nuage de point :
## - On dirait que je ne vois pas de vois pas de tendance linéaire, ce qui me
##   surprend un peu. Je vais quand même continuer avec le reste de l'analyse.
## - On dirait que je vois également des lignes droites à certain montants
##   de réclamations.

modele <- lm (mont_recl ~ valeur_veh, data = donnees)
summary(modele)

## Le modèle serait donc y = 940.66636 - 0.04612x

anova(modele)

## Pour tester si le modèle de régression est significatif, nous utilisons la
## statistique F. Ici, elle est de 53.6 avec 1 et 10 degré de liberté, alors
## la p-value est de P[F > 53.6] = 2.745 x 10^-13.
## Comme la p-value est vraiment plus petite qu 1%, il n'y a pas assez de
## preuve pour rejeter l'hypothèse nulle. Le modèle linéaire simple est donc
## significatif.
##
## Pour R^2, on a 0.8042%. C'est un très bas niveau de corrélation. Ça signifie
## que seulement 0.8042% de la variance des montants de réclamation est expliquée
## par la valeur du véhicule. Comme je semblais le voir au départ, le modèle linéaire
## ne semble pas fonctionné dans ce cas.

boxcox(mont_recl ~ valeur_veh)