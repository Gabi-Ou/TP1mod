<<<<<<< HEAD
## Travail Pratique en Modèles linéaires

x <- read.csv('MontantReclFr.csv')
attach(x)


library(ggplot2)


# Variables de Mél:
x <- read.csv('MontantReclFr.csv')
attach(x)

# Correction des erreurs
mont_recl[mont_recl == -7682.59] <- 7682.59
mont_recl[is.na(mont_recl)]  # Pas de NA dans les montants

# Préparation des données

log_mont_recl <- log(mont_recl)


# stat_matri
unique(stat_matri)    # "Autre" "Seul"

boxplot(log_mont_recl~stat_matri, data= x)

hist(log_mont_recl[stat_matri=="Autre"])
hist(log_mont_recl[stat_matri=="Seul"])

ggplot(x,aes(x = log_mont_recl,fill=stat_matri)) + geom_density(alpha=0.5)

summary(mont_recl[stat_matri=="Autre"])
summary(mont_recl[stat_matri=="Seul"])




# categ_socio
sort(unique(categ_socio))
# "CSP1"  "CSP2" "CSP3"
# "CSP20" "CSP21" "CSP22" "CSP26"
# "CSP37" "CSP40" "CSP42" "CSP46" "CSP47" "CSP48" "CSP49" "CSP50"

boxplot(log_mont_recl~categ_socio, data= x, lex.order = TRUE)
# summary(mont_recl[categ_socio == "CSP1" | categ_socio == "CSP2" | categ_socio == "CSP3"])

ggplot(x,aes(x = log_mont_recl,fill=categ_socio)) + geom_density(alpha=0.5)

# usage_veh
unique(usage_veh) # "professionnel"  "perso + travail"  "perso" "voyagement professionel"


boxplot(log_mont_recl~usage_veh, data= x)

par(mfrow=c(1,2))

hist(log_mont_recl[usage_veh=="professionnel"])
hist(log_mont_recl[usage_veh=="perso + travail"])
hist(log_mont_recl[usage_veh=="perso"])
hist(log_mont_recl[usage_veh=="voyagement professionel"])

summary(mont_recl[usage_veh=="professionnel"])
summary(mont_recl[usage_veh=="perso + travail"])
summary(mont_recl[usage_veh=="perso"])
summary(mont_recl[usage_veh=="voyagement professionel"])

ggplot(x,aes(x = log_mont_recl,fill=usage_veh)) + geom_density(alpha=0.5)

sort(mont_recl)

