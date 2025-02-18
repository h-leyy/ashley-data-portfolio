---
  title: "Analyse de la viralité des vidéos TikTok"
author: "Ashley KILOLA MAKUIZA"
date: "`r Sys.Date()`"
output: html_document
---
##  Introduction : Cette étude analyse les facteurs influençant la viralité des vidéos TikTok à l’aide de modèles économétriques en R.

## Importation des bibliothèques


# Charger les données 
library(readxl)

dataset_tiktok <- read_excel("mémoire/dataset_tiktok.xlsx", 
                             col_types = c("text", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "numeric", "numeric", 
                                           "numeric", "text", "text", "date", 
                                           "text", "numeric", "numeric", "numeric"))

# Modifier la 13ᵉ colonne pour ne garder que l'heure
dataset_tiktok[[13]] <- format(dataset_tiktok[[13]], format = "%H:%M:%S")

# Visualiser les données pour vérifier
View(dataset_tiktok)

# transformer les heures en heures décimales 
dataset_tiktok$`Heure de Publication` <- sapply(dataset_tiktok$`Heure de Publication`, function(x) {
  parts <- as.numeric(unlist(strsplit(x, ":"))) # Séparer heures, minutes, secondes
  parts[1] + parts[2] / 60 + parts[3] / 3600 # Convertir en heures décimales
})

# Visualiser le résultat
View(dataset_tiktok)



#Analyse des variables
#quantifier la relation entre tes variables stratégiques et tes indicateurs clés (abonnés, engagement).
#Les variables timing et contenu influencent-elles les abonnés/engagement?
#Les variables liées à la durée et la fidélisation influencent-elles la rétention?
#pour savoir ce qui marche bien et inversement


#a- les variables temporelles et type de contenu

#1. variable numérique : heure et fréquence


# Corrélation entre heure de publication et abonnés gagnés puis avec l'engagement

#### avec abonnée gagné

# Créer des intervalles d'heures personnalisés
dataset_tiktok$intervalle_heure <- cut(dataset_tiktok$`Heure de Publication`, 
                             breaks = c(0, 7, 10, 13, 16, 19, 24), 
                             labels = c("00h-07h", "07h-10h", "10h-13h", "13h-16h", "16h-19h", "19h-00h"), 
                             right = FALSE)

View(dataset_tiktok)

# Calculer les abonnés moyens gagnés par intervalle horaire
abonnés_moyens <- aggregate(dataset_tiktok$`Abonnés Gagnés` ~ dataset_tiktok$intervalle_heure, data = dataset_tiktok, FUN = mean)

print(abonnés_moyens)

# Bar plot pour afficher les abonnés moyens gagnés par intervalle horaire
ggplot(abonnés_moyens, aes(x = `dataset_tiktok$intervalle_heure`, y = `dataset_tiktok$\`Abonnés Gagnés\``, fill = `dataset_tiktok$intervalle_heure`)) +
  geom_bar(stat = "identity") +
  labs(title = "Abonnés moyens gagnés par intervalles d'heure de publication",
       x = "Intervalle d'heure",
       y = "Abonnés moyens gagnés") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")
  

## ccl : en moyenne publier entre 13h/16h et 19h/00h génére plus d'abonnés

moyenne_engagement <- aggregate(dataset_tiktok$Engagement ~ dataset_tiktok$intervalle_heure, data = dataset_tiktok, FUN = mean)

# Afficher les résultats
print(moyenne_engagement)


ggplot(moyenne_engagement, aes(x = `dataset_tiktok$intervalle_heure`, y = `dataset_tiktok$Engagement`, fill = `dataset_tiktok$intervalle_heure`)) +
 geom_bar(stat = "identity") +
  labs(title = "Moyenne de l'engagement par plage horaire",
       x = "Plage horaire",
       y = "Moyenne de l'engagement") +
  theme_minimal() +
   scale_fill_brewer(palette = "Set3")


# publier en 10h et 00h


# abonné et engagement par rapport à l'heure de publication
moyenne_abonnés_engagement <- aggregate(cbind(dataset_tiktok$`Abonnés Gagnés`, dataset_tiktok$Engagement) ~ dataset_tiktok$intervalle_heure, 
data = dataset_tiktok, FUN = mean)

print(moyenne_abonnés_engagement)


## en confondant les résultats : publier entre 13h-16h et 19h-00h 



# fréquence de publication

summary(dataset_tiktok$`Fréquence de Publication (j)`)


# sur les abonnés


# Diagramme en barres groupées
ggplot(dataset_tiktok, aes(x = dataset_tiktok$`Fréquence de Publication (j)`, y = dataset_tiktok$`Abonnés Gagnés`, fill = dataset_tiktok$`Fréquence de Publication (j)`)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Nombre moyen d'abonnés gagnés par fréquence de publication",
       x = "Fréquence de publication",
       y = "Nombre moyen d'abonnés",
       fill = "Fréquence") +
  theme_minimal()

# ccl : vidéo publié à 0/1/2/4/20 ont amené plus 



# Calculer le nombre moyen de vidéos par intervalle de fréquence

# Sélectionner les colonnes pertinentes
dataset_tiktok_abonnés <- dataset_tiktok[, c("Abonnés Gagnés", "Fréquence de Publication (j)")]

# Calculer le nombre moyen de vidéos par fréquence de publication
moyenne_videos_par_intervalle <- aggregate(
  `Abonnés Gagnés` ~ `Fréquence de Publication (j)`, 
  data = dataset_tiktok, 
  FUN = function(x) length(x)
)

# Renommer les colonnes pour simplifier
colnames(moyenne_videos_par_intervalle) <- c("Fréquence", "Vidéos Moyennes")

# Conversion de la variable "Fréquence" en facteur
moyenne_videos_par_intervalle$Fréquence <- as.factor(moyenne_videos_par_intervalle$Fréquence)

# Vérifier le nombre de niveaux pour ajuster les couleurs
if (length(unique(moyenne_videos_par_intervalle$Fréquence)) > 8) {
  # Utiliser une palette générée si >8 niveaux
  colors <- scales::hue_pal()(length(unique(moyenne_videos_par_intervalle$Fréquence)))
} else {
  # Palette Set2 si ≤8 niveaux
  colors <- RColorBrewer::brewer.pal(length(unique(moyenne_videos_par_intervalle$Fréquence)), "Set2")
}

# Diagramme en barres
library(ggplot2)
ggplot(moyenne_videos_par_intervalle, aes(x = Fréquence, y = `Vidéos Moyennes`, fill = Fréquence)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Nombre moyen de vidéos qui ont gagné des abonnés par fréquence de publication",
    x = "Fréquence de publication (jours)",
    y = "Nombre moyen de vidéos",
    fill = "Fréquence"
  ) +
  theme_minimal()


#ccl les vidéos qui sont publié à un intervalle de 0/1/2 approter plus d'abonnés



# sur l'engagement
# Diagramme en barres groupées
ggplot(dataset_tiktok, aes(x = dataset_tiktok$`Fréquence de Publication (j)`, y = dataset_tiktok$Engagement, fill = dataset_tiktok$`Fréquence de Publication (j)`)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Engagement moyen par fréquence de publication",
       x = "Fréquence de publication",
       y = "Engagement moyen",
       fill = "Fréquence") +
  theme_minimal()

# ccl : vidéo publié à 0/1/2/4/20 ont amené plus d'eng


# Calculer le nombre moyen de vidéos avec engagement par fréquence de publication
moyenne_videos_par_intervalle_eng <- aggregate(
  Engagement ~ `Fréquence de Publication (j)`, 
  data = dataset_tiktok, 
  FUN = function(x) length(x)
)

# Renommer les colonnes pour simplifier
colnames(moyenne_videos_par_intervalle_eng) <- c("Fréquence", "Vidéos Moyennes")

# Conversion de la variable "Fréquence" en facteur
moyenne_videos_par_intervalle_eng$Fréquence <- as.factor(moyenne_videos_par_intervalle_eng$Fréquence)

# Vérifier le nombre de niveaux pour ajuster les couleurs
if (length(unique(moyenne_videos_par_intervalle_eng$Fréquence)) > 8) {
  # Utiliser une palette générée si >8 niveaux
  colors <- scales::hue_pal()(length(unique(moyenne_videos_par_intervalle_eng$Fréquence)))
} else {
  # Palette Set2 si ≤8 niveaux
  colors <- RColorBrewer::brewer.pal(length(unique(moyenne_videos_par_intervalle_eng$Fréquence)), "Set2")
}

# Diagramme en barres
library(ggplot2)
ggplot(moyenne_videos_par_intervalle_eng, aes(x = Fréquence, y = `Vidéos Moyennes`, fill = Fréquence)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = colors) +
  labs(
    title = "Nombre moyen de vidéos qui ont généré de l'engagement par fréquence de publication",
    x = "Fréquence de publication (jours)",
    y = "Nombre moyen de vidéos",
    fill = "Fréquence"
  ) +
  theme_minimal()


#ccl les vidéos qui sont plublé à un intervalle de 0/1/2 approter plus d'eng



#2. variable catégorique : jour et type de contenu

#### abonnés


# Chargement des bibliothèques nécessaires
library(ggplot2)
library(dplyr)
library(RColorBrewer)

# 1. Visualisation par Jour de Publication
ggplot(dataset_tiktok, aes(x = `Jour de Publication`, y = `Abonnés Gagnés`, fill = `Jour de Publication`)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Nombre d'abonnés gagnés par jour de publication",
       x = "Jour de la semaine", y = "Nombre moyen d'abonnés gagnés") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# ccl jeudi mardi et vendredi m'amène plus d'abonnés


# 2. Visualisation par Type de Contenu
ggplot(dataset_tiktok, aes(x = `Type de Contenu`, y = `Abonnés Gagnés`, fill = `Type de Contenu`)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Nombre d'abonnés gagnés par type de contenu",
       x = "Type de contenu", y = "Nombre moyen d'abonnés gagnés") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


#ccl débat, avis, conseil donne plus d'abonnés


# Visualisation de l'interaction entre Jour de Publication et Type de Contenu
ggplot(dataset_tiktok, aes(x = `Jour de Publication`, y = `Abonnés Gagnés`, color = `Type de Contenu`, group = `Type de Contenu`)) +
  geom_line(stat = "summary", fun = "mean") +
  geom_point(stat = "summary", fun = "mean") +
  labs(title = "Interaction entre jour de publication et type de contenu sur les abonnés gagnés",
       x = "Jour de la semaine", y = "Nombre moyen d'abonnés gagnés") +
  theme_minimal()

# en croisant les deux, ce qui sort du lot c'est que mardi je poste plus les débat


##### engagement


# 1. Visualisation par Jour de Publication
ggplot(dataset_tiktok, aes(x = `Jour de Publication`, y = dataset_tiktok$Engagement, fill = `Jour de Publication`)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Engagement par jour de publication",
       x = "Jour de la semaine", y = "Engagement") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# ccl samedi vendredi mardi dimanche mercredi


# 2. Visualisation par Type de Contenu
ggplot(dataset_tiktok, aes(x = `Type de Contenu`, y = dataset_tiktok$Engagement, fill = `Type de Contenu`)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Engagement  par type de contenu",
       x = "Type de contenu", y = "Engagement") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")


#ccl débat conseil manipulation trend


# Visualisation de l'interaction entre Jour de Publication et Type de Contenu
ggplot(dataset_tiktok, aes(x = `Jour de Publication`, y = dataset_tiktok$Engagement, color = `Type de Contenu`, group = `Type de Contenu`)) +
  geom_line(stat = "summary", fun = "mean") +
  geom_point(stat = "summary", fun = "mean") +
  labs(title = "Interaction entre jour de publication et type de contenu sur l'engagement",
       x = "Jour de la semaine", y = "Engagement") +
  theme_minimal()

# en croisant les deux





# 2e catégorie de corrélation : visionnage moyen, % à la fin, fidélisation

#### type de contenu


# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)

# Analyse descriptive : Moyenne, médiane, écart-type pour la fidélisation, le visionnage moyen et la durée de la vidéo
# Vérifier les noms des colonnes disponibles dans dataset_tiktok
colnames(dataset_tiktok)

# Afficher un résumé des variables spécifiques du dataframe
summary(dataset_tiktok[, c("Taux de Fidélisation %", "Temps Visionnage Moyen (s)", "Durée Vidéo (s)", "% Regardé Jusqu'à la Fin")])

## faire des commentaires sur les 3 variables qu'on cherche à corréler

### type de contenu

# Moyenne de fidélisation, visionnage moyen, et pourcentage de personnes par type de contenu
aggregate(cbind(`Taux de Fidélisation %`, `Temps Visionnage Moyen (s)`, `Durée Vidéo (s)`, `% Regardé Jusqu'à la Fin`) ~ `Type de Contenu`, 
          data = dataset_tiktok, FUN = mean)


#fidélisation


# Calcul de la moyenne de la fidélisation par type de contenu
moyenne_fidélisation <- aggregate(`Taux de Fidélisation %` ~ `Type de Contenu`, data = dataset_tiktok, FUN = mean)

# Visualisation : Diagramme en barres de la fidélisation moyenne par type de contenu
ggplot(moyenne_fidélisation, aes(x = `Type de Contenu`, y = `Taux de Fidélisation %`, fill = `Type de Contenu`)) +
  geom_bar(stat = "identity") +
  labs(title = "Fidélisation moyenne par Type de Contenu", x = "Type de Contenu", y = "Fidélisation Moyenne") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


#ccl trend débat manipulation conseil


## visionnage moyen

# Calcul de la moyenne du temps de visionnage moyen par type de contenu
visionnage_mooyen <- aggregate(`Temps Visionnage Moyen (s)` ~ `Type de Contenu`, data = dataset_tiktok, FUN = mean)

# Visualisation : Diagramme en barres de la fidélisation moyenne par type de contenu
ggplot(visionnage_mooyen, aes(x = `Type de Contenu`, y = `Temps Visionnage Moyen (s)`, fill = `Type de Contenu`)) +
  geom_bar(stat = "identity") +
  labs(title = "Visionnage moyen par Type de Contenu", x = "Type de Contenu", y = "Visionnage Moyenne") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#ccl débat avis conseil


# % regardé jusqu'à la fin 

# Calcul de la moyenne du temps de visionnage moyen par type de contenu
fin_moyen <- aggregate(`% Regardé Jusqu'à la Fin` ~ `Type de Contenu`, data = dataset_tiktok, FUN = mean)

# Visualisation : Diagramme en barres de la visualisation jusqu'à la fin moyenne par type de contenu
ggplot(fin_moyen, aes(x = `Type de Contenu`, y = `% Regardé Jusqu'à la Fin`, fill = `Type de Contenu`)) +
  geom_bar(stat = "identity") +
  labs(title = "à la fin par Type de Contenu", x = "Type de Contenu", y = "à la fin") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

#ccl trend débat manipulation conseil


#### durée de la vidéo 

# Créer une variable catégorielle pour les intervalles de durée de vidéo
dataset_tiktok$durée_catégorie <- cut(dataset_tiktok$`Durée Vidéo (s)`,
                                      breaks = c(0, 30, 60, 90, 120, Inf),
                                      labels = c("0-30s", "30s-1min", "1min-1min30", "1min30-2min", "2min+"))



### fidélisation


# Charger les bibliothèques nécessaires
library(ggplot2)
library(dplyr)


# Calculer la moyenne de fidélisation par intervalle de durée
moyenne_fidélisation <- dataset_tiktok %>%
  group_by(durée_catégorie) %>%
  summarise(moyenne_fidélisation = mean(`Taux de Fidélisation %`, na.rm = TRUE))

# Visualisation : Diagramme en barres de la fidélisation moyenne par durée de vidéo
ggplot(moyenne_fidélisation, aes(x = durée_catégorie, y = moyenne_fidélisation, fill = durée_catégorie)) +
  geom_bar(stat = "identity") +
  labs(title = "Fidélisation Moyenne par Durée de Vidéo", 
       x = "Durée de Vidéo", y = "Fidélisation Moyenne (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


#ccl vidéo max 1m voir 1m30




### visionnage moyen

# Identifier les vidéos avec le visionnage moyen le plus élevé par intervalle de durée
top_videos <- dataset_tiktok %>%
  group_by(durée_catégorie) %>%
  slice_max(`Temps Visionnage Moyen (s)`, n = 1)

# Diagramme en barres
ggplot(top_videos, aes(x = durée_catégorie, y = `Temps Visionnage Moyen (s)`, fill = durée_catégorie)) +
  geom_bar(stat = "identity") +
  labs(title = "Vidéos avec le Visionnage Moyen le Plus Élevé par Intervalle de Durée",
       x = "Durée de Vidéo (catégories)",
       y = "Visionnage Moyen (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")

#ccl 1m/1m30 parfait






### % à la fin

# Créer une variable catégorielle pour les intervalles de durée de vidéo
dataset_tiktok$durée_catégorie <- cut(dataset_tiktok$`Durée Vidéo (s)`,
                                      breaks = c(0, 30, 60, 90, 120, Inf),
                                      labels = c("0-30s", "30s-1min", "1min-1min30", "1min30-2min", "2min+"))

# Calculer le pourcentage moyen regardé jusqu'à la fin par catégorie de durée de vidéo
pourcentage_fin <- dataset_tiktok %>%
  group_by(durée_catégorie) %>%
  summarise(pourcentage_fin = mean(`% Regardé Jusqu'à la Fin`, na.rm = TRUE))

# Visualisation : Diagramme en barres
ggplot(pourcentage_fin, aes(x = durée_catégorie, y = pourcentage_fin, fill = durée_catégorie)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Pourcentage Regardé Jusqu'à la Fin par Durée de Vidéo",
       x = "Durée de Vidéo (catégories)", 
       y = "Pourcentage Moyenne (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotation des labels de l'axe x

#ccl max 1m/1m30





## ccl en croisant les analyses descriptives :

## analyse des corrélations

##### temporelle
########## nombre d'abonné
############### numérique
#################### heure : 13h/16h 19h/00h
#################### fréquence : 0/1/2
############### character
#################### date : jeudi mardi vendredi
#################### type : débat avis conseil
########## engagement 
############### numérique
#################### heure : 10h/00h
#################### fréquence : 0/1/2/4
############### character
#################### date : samedi vendredi mardi dimanche mercredi
#################### type : débat conseil manipulation trend



##### visionnage
########## visonnage moyen
############### type de contenu : débat avis conseil
############### durée video : 1m/1m30
########## fidélisation 
############### type de contenu : trend débat manipulation conseil
############### durée video : 1m/1m30
########## jusqu'à la fin
############### type de contenu : trend débat manipulation conseil
############### durée video : 1m/1m30

# les ccl :

# intervalle de publication : 13h/16h et 19h/00h
# fréquence de publication : 0/1 jour d'intervalle
# jours à éviter : éviter lundi
# type de contenu à éviter : éviter haul et test
# durée de vidéo : 1m/1m30
























#Régression linéaire multiple
#isoler l’effet individuel de chaque variable tout en tenant compte des interactions.
#facteurs qui impactent directement la croissance du nb d'abonné
#"Une augmentation de la fréquence de publication de 1 fois/semaine entraîne un gain moyen de X abonnés."
#"Les vidéos éducatives génèrent en moyenne Y abonnés de plus que les vidéos humoristiques."




# Régression linéaire pour prédire les abonnés gagnés
model <- lm(`Abonnés Gagnés` ~ `Type de Contenu` + `Durée Vidéo (s)` + `Heure de Publication` + `Jour de Publication`, 
            data = dataset_tiktok)

summary(model)





#Prédictions sur les variables actionnables
# scénarios hypothétiques, tu peux simuler l’impact d’ajuster les variables sur lesquelles tu as du contrôle
#formuler une stratégie actionnable pour optimiser ta croissance d'abonnés.
#Développer des recommandations spécifiques :
#"Publier des vidéos éducatives courtes 3 fois par semaine à 19h augmentera les abonnés de 20 %.



