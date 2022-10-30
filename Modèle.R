install.packages('naniar')
install.packages('rlang')
install.packages('leaps')

Original <- read.csv('MontantReclFr.csv')
x <- read.csv('MontantReclFr.csv')

attach(x)
library(ggplot2)
library(leaps)
library(car)
# Note sur le traitement des données

# summary(influence.measures(modele)) # Cette commande donne les mesures d'influences d'un modèle donné. p.83 et 99

mont_recl[is.na(mont_recl)]                   # Pas de NA dans les montants
mont_recl[mont_recl<=0]

mont_recl[mont_recl == -7682.59] <- 7682.59   # Changement de la valeur.
mont_recl[mont_recl == -7682.59]              # Il est bien parti.
# mont_recl[mont_recl < 50] <- NA             # À voir ce qu'on fait avec les valeurs faibles avec les données influentes



age_veh[age_veh > 50]             # Trois Données: 160, 160, 159.
x[age_veh > 20, ]                 # Lignes complètes de ces données.
x[age_veh > 20, ]$age_veh <- NA   # On converti en NA.
is.na(x[age_veh > 20, ]$age_veh)  # On vérifie que les données sont bien des NA.
x[age_veh > 20, ]                 # On revalide le tout sur la ligne.


bonus_malus[bonus_malus < 50 | bonus_malus >= 350]            # Deux données: -5, -5.
x[bonus_malus < 50 | bonus_malus >= 350, ]                    # Lignes complètes de ces données.
x[bonus_malus < 50 | bonus_malus >= 350, ]$bonus_malus <- NA  # On converti en NA.
is.na(x[bonus_malus < 50 | bonus_malus >= 350, ]$bonus_malus) # On vérifie que les données sont bien des NA.
x[bonus_malus < 50 | bonus_malus >= 350, ]                    # On revalide le tout sur la ligne.

table(genre)
genre_2 <- sub("femme", "Femme", genre)
genre <- genre_2
table(genre)


# Modèle

colnames(x)  # Donne toutes les variables.




# Premier Test: Toutes les variables, avec transformation de mont_recl.
# On regarde le meilleur modèle en fonction du nombre de variables.

log_mont_recl <- log(mont_recl)
log_valeur_veh <- log(valeur_veh)

tous <- regsubsets(I(log_mont_recl)~.-valeur_veh + log_valeur_veh, data=x, nvmax=15, nbest=1)  # ATTENTION DEVIENT LENT À PARTIR DE 10.
tous.summary <- summary(tous)

# On peut se faire un tableau avec les différentes valeurs des critères de sélection pour le
# meilleur modèle de chaque taille. p.119
npar <- rowSums(tous.summary$which)
Criteres <- data.frame(npar, Rcarre=tous.summary$rsq,
                       Rcarre.adj=tous.summary$adjr2,Cp=tous.summary$cp,
                       BIC=tous.summary$bic,
                       AIC=tous.summary$bic +npar*(2-log(nrow(x))))
round(Criteres,3)

# Deuxième Test : Toutes les variables, avec transformation de mont_recl.
# On regarde le meilleur modèle avec les méthodes algorithmiques.

CompletSimple <- lm(I(log_mont_recl) ~ age_permi + age_veh + genre + stat_matri + categ_socio + usage_veh + age_conduct + type_veh + bonus_malus + garage + source_energ + I(log(valeur_veh)), data = x)
Reduit1 <- lm(I(log_mont_recl) ~ age_permi + age_veh + genre + stat_matri + categ_socio + usage_veh + age_conduct + type_veh + bonus_malus + garage + source_energ + I(log(valeur_veh)), data = x)

MOD1valeurveh <- lm(I(log_mont_recl) ~ I(log(valeur_veh)))
summary(MOD1valeurveh)$sigma
summary(MOD1valeurveh)$df[2]

MOD1categ_socio <- lm(I(log_mont_recl) ~ categ_socio)
((summary(MOD1categ_socio)$sigma)^2) * summary(MOD1categ_socio)$df[2]
summary(MOD1categ_socio)$df

MOD1usage_veh <- lm(I(log_mont_recl) ~ usage_veh)
((summary(MOD1usage_veh)$sigma)^2) * summary(MOD1usage_veh)$df[2]
summary(MOD1usage_veh)$df

MOD1valeurveh <- lm(I(log_mont_recl) ~ usage_veh)
((summary(MOD1usage_veh)$sigma)^2) * summary(MOD1valeurveh)$df[2]
summary(MOD1valeurveh)$df



ModeleDeBase <- lm(I(log_mont_recl) ~ bonus_malus + I(log(valeur_veh)) + genre + age_conduct + age_conduct*genre)
summary(ModeleDeBase)
table(genre)

summary(lm(I(log_mont_recl) ~ age_veh) )


ModeleComplet <- step(Complet, direction = "both", test="F", trace=1)
#ModeleComplet <- step(Complet, direction = "forward", test="F", trace=1)
#ModeleComplet <- step(Complet, direction = "backward", test="F", trace=1)





