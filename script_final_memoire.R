# MÉMOIRE M1 BIDABI – ANALYSE DE LA VIRALITÉ SUR TIKTOK =====
# Étudiante : Ashley KILOLA MAKUIZA
# Objectifs : Nettoyage, transformation et analyse du dataset d'un compte TikTok



## Importation des bibliothèques

library(readxl)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(mice)
library(DMwR2)
library(tidyverse)

# Préparation des données ----

# Charger les données 

dataset_tiktok_ajusté <- read_excel("~/mémoire/dataset_tiktok_ajusté.xlsx", 
                                    col_types = c("text", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric", "text", "text", "date", 
                                                  "text", "numeric", "numeric", "numeric", 
                                                  "numeric"))


View(dataset_tiktok_ajusté)


# Modifier la 13ᵉ colonne pour ne garder que l'heure
dataset_tiktok_ajusté[[13]] <- format(dataset_tiktok_ajusté[[13]], format = "%H:%M:%S")


# transformer les heures en heures décimales 
dataset_tiktok_ajusté$`Heure de Publication` <- sapply(dataset_tiktok_ajusté$`Heure de Publication`, function(x) {
  parts <- as.numeric(unlist(strsplit(x, ":"))) 
  parts[1] + parts[2] / 60 + parts[3] / 3600 
}) 

# Visualiser les données pour vérifier
View(dataset_tiktok_ajusté)

# Appliquer One-Hot Encoding aux variables catégorielles
dataset_tiktok_clean <- dataset_tiktok_ajusté

# One-Hot Encoding pour "Jour de Publication" et "Type de Contenu"
dataset_tiktok_clean <- cbind(dataset_tiktok_clean, 
                              model.matrix(~ `Jour de Publication` - 1, data = dataset_tiktok_clean),
                              model.matrix(~ `Type de Contenu` - 1, data = dataset_tiktok_clean))

# Vérifier les nouvelles colonnes après One-Hot Encoding
View(dataset_tiktok_clean)


# Vérifier que la colonne est bien numérique
str(dataset_tiktok_clean$`Heure de Publication`)  # Doit afficher "num"

# Appliquer les intervalles horaires
dataset_tiktok_clean$intervalle_heure <- cut(dataset_tiktok_clean$`Heure de Publication`, 
                                             breaks = c(0, 7, 10, 13, 16, 19, 24), 
                                             labels = c("00h-07h", "07h-10h", "10h-13h", "13h-16h", "16h-19h", "19h-00h"), 
                                             right = FALSE)


# Appliquer One-Hot Encoding aux intervalles horaires
one_hot_intervals <- model.matrix(~ intervalle_heure - 1, data = dataset_tiktok_clean)

# Ajouter les variables One-Hot Encoding au dataset
dataset_tiktok_clean <- cbind(dataset_tiktok_clean, one_hot_intervals)

View(dataset_tiktok_clean)


colnames(dataset_tiktok_clean) <- gsub("`", "", colnames(dataset_tiktok_clean))  # Supprimer les backticks
colnames(dataset_tiktok_clean) <- gsub(" ", "_", colnames(dataset_tiktok_clean))  # Remplacer les espaces par des underscores

# Vérifier le résultat
colnames(dataset_tiktok_clean)  # Affiche les nouveaux noms des colonnes
head(dataset_tiktok_clean)  # Vérifie les premières lignes du dataset

# Nombre total d’observations
n_obs <- nrow(dataset_tiktok_clean)
print(paste("Nombre total d’observations :", n_obs))


# Analyse descriptive ----

# partie 1 : variables cibles ----

# Abonnés gagnés
summary(dataset_tiktok_clean$Abonnés_Gagnés)
sd(dataset_tiktok_clean$Abonnés_Gagnés, na.rm = TRUE)

# Boxplot – Abonnés gagnés pour visualiser la valeur extrême
ggplot(dataset_tiktok_clean, aes(y = Abonnés_Gagnés)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Valeurs extrêmes - Abonnés gagnés") +
  theme_minimal()

# Engagement
summary(dataset_tiktok_clean$Engagement)
sd(dataset_tiktok_clean$Engagement, na.rm = TRUE)

# Densité – Engagement pour comprendre la distribution
ggplot(dataset_tiktok_clean, aes(x = Engagement)) +
  geom_density(fill = "lightblue") +
  labs(title = "Densité de l'engagement", x = "Engagement") +
  theme_minimal()

# Taux de fidélisation
summary(dataset_tiktok_clean$`Temps_Visionnage_Moyen_(s)`)
sd(dataset_tiktok_clean$`Taux_de_Fidélisation_%`, na.rm = TRUE)

# Temps moyen d'écoute
summary(dataset_tiktok_clean$`Temps_Visionnage_Moyen_(s)`)
sd(dataset_tiktok_clean$`Temps_Visionnage_Moyen_(s)`, na.rm = TRUE)



# partie 2 : Comportement éditorial ----

# CLASSEMENT DES JOURS DE PUBLICATION 
dataset_tiktok_clean %>%
  count(Jour_de_Publication, sort = TRUE) %>%
  mutate(Jour_de_Publication = factor(Jour_de_Publication, levels = c("LUNDI", "MARDI", "MERCREDI", "JEUDI", "VENDREDI", "SAMEDI", "DIMANCHE"))) %>%
  ggplot(aes(x = Jour_de_Publication, y = n, fill = Jour_de_Publication)) +
  geom_col() +
  labs(title = "Classement des jours où je publie le plus", y = "Nombre de publications") +
  theme_minimal()

jours_publi <- dataset_tiktok_clean %>%
  count(Jour_de_Publication, sort = TRUE)

print(jours_publi)


# INTERVALLE HORAIRE DE PUBLICATION LE PLUS UTILISÉ
dataset_tiktok_clean %>%
  count(intervalle_heure, sort = TRUE) %>%
  ggplot(aes(x = reorder(intervalle_heure, -n), y = n, fill = intervalle_heure)) +
  geom_col() +
  labs(title = "Intervalle horaire le plus utilisé", x = "Plage horaire", y = "Nombre de publications") +
  theme_minimal()

plages_horaires <- dataset_tiktok_clean %>%
  count(intervalle_heure, sort = TRUE)

print(plages_horaires)


# TYPE DE CONTENU LE PLUS PUBLIÉ
dataset_tiktok_clean %>%
  count(Type_de_Contenu, sort = TRUE) %>%
  ggplot(aes(x = reorder(Type_de_Contenu, -n), y = n, fill = Type_de_Contenu)) +
  geom_col() +
  labs(title = "Type de contenu le plus publié", x = "Type de contenu", y = "Nombre de vidéos") +
  theme_minimal()

types_contenu <- dataset_tiktok_clean %>%
  count(Type_de_Contenu, sort = TRUE)

print(types_contenu)


# ANALYSE DE la FRÉQUENCE DE PUBLICATION

ggplot(dataset_tiktok_clean, aes(x = `Fréquence_de_Publication_(j)`)) +
  geom_histogram(binwidth = 1, fill = "darkorchid", color = "white") +
  labs(
    title = "Analyse de la régularité : Fréquence de publication",
    x = "Nombre de jours entre deux vidéos",
    y = "Nombre de cas"
  ) +
  theme_minimal()

summary(dataset_tiktok_clean$`Fréquence_de_Publication_(j)`)




#Analyse exploratoire  ----

#Abonnés gagnés vs Variables éditoriales  ----


# Créneau de publication


abonnés_heure <- aggregate(Abonnés_Gagnés ~ intervalle_heure, data = dataset_tiktok_clean, mean)

ggplot(abonnés_heure, aes(x = intervalle_heure, y = Abonnés_Gagnés, fill = intervalle_heure)) +
  geom_bar(stat = "identity") +
  labs(title = "Abonnés gagnés selon le créneau horaire", x = "Créneau", y = "Abonnés gagnés (moyenne)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

abon_heure_total <- aggregate(Abonnés_Gagnés ~ intervalle_heure, data = dataset_tiktok_clean, sum)
print(abon_heure_total)


# Fréquence de publication

ggplot(dataset_tiktok_clean, aes(x = `Fréquence_de_Publication_(j)`, 
                                 y = Abonnés_Gagnés, 
                                 fill = `Fréquence_de_Publication_(j)`)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Abonnés gagnés selon la fréquence de publication", 
       x = "Fréquence (j)", 
       y = "Moyenne d'abonnés") +
  theme_minimal()

abon_freq_total <- aggregate(Abonnés_Gagnés ~ `Fréquence_de_Publication_(j)`, data = dataset_tiktok_clean, sum)
print(abon_freq_total)

# Jour de publication

dataset_tiktok_clean %>%
  mutate(Jour_de_Publication = factor(Jour_de_Publication, 
                                      levels = c("LUNDI", "MARDI", "MERCREDI", "JEUDI", "VENDREDI", "SAMEDI", "DIMANCHE"))) %>%
  ggplot(aes(x = Jour_de_Publication, y = Abonnés_Gagnés, fill = Jour_de_Publication)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Abonnés gagnés selon le jour de publication", x = "Jour", y = "Moyenne d'abonnés") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")


abon_jour_total <- aggregate(Abonnés_Gagnés ~ Jour_de_Publication, data = dataset_tiktok_clean, sum)
print(abon_jour_total)


# Type de contenu

ggplot(dataset_tiktok_clean, aes(x = Type_de_Contenu, y = Abonnés_Gagnés, fill = Type_de_Contenu)) +
  geom_bar(stat = "summary", fun = "mean") +
  labs(title = "Abonnés gagnés selon le type de contenu", x = "Type", y = "Moyenne d'abonnés") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")

abon_type_total <- aggregate(Abonnés_Gagnés ~ Type_de_Contenu, data = dataset_tiktok_clean, sum)
print(abon_type_total)





# Taux d’engagement vs Variables éditoriales ----

# Créneau de publication 


engagement_heure_pondere <- dataset_tiktok_clean %>%
  group_by(intervalle_heure) %>%
  summarise(engagement_pondere = sum(Engagement * nb_de_Vues) / sum(nb_de_Vues))

print(engagement_heure_pondere)

ggplot(engagement_heure_pondere, aes(x = intervalle_heure, y = engagement_pondere, color = intervalle_heure)) +
  geom_point(size = 3) +
  geom_line(group = 1) +   
  labs(title = "Engagement moyen pondéré selon l’heure de publication",
       x = "Intervalle horaire",
       y = "Engagement moyen pondéré") +
  theme_minimal() +
  theme(legend.position = "none")  


# Fréquence de publication 

engagement_freq_pondere <- dataset_tiktok_clean %>%
  group_by(`Fréquence_de_Publication_(j)`) %>%
  summarise(engagement_pondere = sum(Engagement * nb_de_Vues) / sum(nb_de_Vues))

print(engagement_freq_pondere)

# Visualisation de l’engagement moyen pondéré en fonction de la fréquence de publication
ggplot(engagement_freq_pondere, aes(x = `Fréquence_de_Publication_(j)`, y = engagement_pondere)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Engagement moyen pondéré selon la fréquence de publication",
       x = "Fréquence de publication (jours)",
       y = "Engagement moyen pondéré") +
  theme_minimal()



# Jour de publication 


engagement_jour_pondere <- dataset_tiktok_clean %>%
  mutate(Jour_de_Publication = factor(Jour_de_Publication, 
                                      levels = c("LUNDI", "MARDI", "MERCREDI", "JEUDI", "VENDREDI", "SAMEDI", "DIMANCHE"))) %>%
  group_by(Jour_de_Publication) %>%
  summarise(engagement_pondere = sum(Engagement * nb_de_Vues) / sum(nb_de_Vues))

print(engagement_jour_pondere)

ggplot(engagement_jour_pondere, aes(x = Jour_de_Publication, y = engagement_pondere, fill = Jour_de_Publication)) +
  geom_bar(stat = "identity") +
  labs(title = "Engagement moyen pondéré selon le jour de publication", x = "Jour", y = "Engagement moyen pondéré") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")

# Type de contenu 

engagement_type_pondere <- dataset_tiktok_clean %>%
  group_by(Type_de_Contenu) %>%
  summarise(engagement_pondere = sum(Engagement * nb_de_Vues) / sum(nb_de_Vues))

print(engagement_type_pondere)


ggplot(engagement_type_pondere, aes(x = Type_de_Contenu, y = engagement_pondere, fill = Type_de_Contenu)) +
  geom_bar(stat = "identity") +
  labs(title = "Engagement moyen pondéré selon le type de contenu",
       x = "Type de contenu", y = "Engagement moyen pondéré") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")




# Taux de fidélisation vs Variables éditoriales  ----

# Créneau de publication

fidelisation_heure_pondere <- dataset_tiktok_clean %>%
  group_by(intervalle_heure) %>%
  summarise(fidelisation_ponderee = sum(`Taux_de_Fidélisation_%` * nb_de_Vues) / sum(nb_de_Vues))

print(fidelisation_heure_pondere)

ggplot(fidelisation_heure_pondere, aes(x = intervalle_heure, y = fidelisation_ponderee, fill = intervalle_heure)) +
  geom_col() +
  labs(title = "Fidélisation moyenne pondérée selon l’heure de publication", 
       x = "Créneau horaire", 
       y = "Taux de fidélisation moyen (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")


# Fréquence de publication

fidelisation_freq_pondere <- dataset_tiktok_clean %>%
  group_by(`Fréquence_de_Publication_(j)`) %>%
  summarise(fidelisation_ponderee = sum(`Taux_de_Fidélisation_%` * nb_de_Vues) / sum(nb_de_Vues)) %>%
  mutate(fidelisation_ponderee = ifelse(fidelisation_ponderee > 100, 100, fidelisation_ponderee))

print(fidelisation_freq_pondere)


ggplot(fidelisation_freq_pondere, aes(x = `Fréquence_de_Publication_(j)`, y = fidelisation_ponderee)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Fidélisation moyenne pondérée selon la fréquence de publication", 
    x = "Fréquence (jours entre chaque vidéo)", 
    y = "Taux de fidélisation (%)"
  ) +
  theme_minimal()



# Jour de publication

fidelisation_jour_pondere <- dataset_tiktok_clean %>%
  mutate(Jour_de_Publication = factor(Jour_de_Publication, 
                                      levels = c("LUNDI", "MARDI", "MERCREDI", "JEUDI", "VENDREDI", "SAMEDI", "DIMANCHE"))) %>%
  group_by(Jour_de_Publication) %>%
  summarise(fidelisation_ponderee = sum(`Taux_de_Fidélisation_%` * nb_de_Vues) / sum(nb_de_Vues))

print(fidelisation_jour_pondere)

ggplot(fidelisation_jour_pondere, aes(x = Jour_de_Publication, y = fidelisation_ponderee, fill = Jour_de_Publication)) +
  geom_col() +
  labs(title = "Fidélisation moyenne pondérée selon le jour de publication", 
       x = "Jour", y = "Taux de fidélisation moyen (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# Type de contenu
fidelisation_type_pondere <- dataset_tiktok_clean %>%
  group_by(Type_de_Contenu) %>%
  summarise(fidelisation_ponderee = sum(`Taux_de_Fidélisation_%` * nb_de_Vues) / sum(nb_de_Vues))

print(fidelisation_type_pondere)


ggplot(fidelisation_type_pondere, aes(x = Type_de_Contenu, y = fidelisation_ponderee, fill = Type_de_Contenu)) +
  geom_col() +
  labs(title = "Fidélisation moyenne pondérée selon le type de contenu", 
       x = "Type", y = "Taux de fidélisation moyen (%)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1")




# Temps de visionnage moyen vs Variables éditoriales ----

# Créneau de publication

visionnage_heure_pondere <- dataset_tiktok_clean %>%
  group_by(intervalle_heure) %>%
  summarise(temps_moyen_pondere = sum(`Temps_Visionnage_Moyen_(s)` * nb_de_Vues) / sum(nb_de_Vues))

print(visionnage_heure_pondere)

ggplot(visionnage_heure_pondere, aes(x = intervalle_heure, y = temps_moyen_pondere, color = intervalle_heure)) +
  geom_point(size = 3) +
  labs(title = "Temps de visionnage moyen pondéré selon l’heure", x = "Heure", y = "Temps moyen (sec)") +
  theme_minimal()



# Fréquence de publication

visionnage_freq_pondere <- dataset_tiktok_clean %>%
  group_by(`Fréquence_de_Publication_(j)`) %>%
  summarise(temps_moyen_pondere = sum(`Temps_Visionnage_Moyen_(s)` * nb_de_Vues) / sum(nb_de_Vues))

print(visionnage_freq_pondere)


ggplot(visionnage_freq_pondere, aes(x = `Fréquence_de_Publication_(j)`, y = temps_moyen_pondere)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Temps de visionnage moyen pondéré selon la fréquence de publication", 
       x = "Fréquence (jours entre chaque vidéo)", 
       y = "Temps moyen (sec)") +
  theme_minimal()


# Jour de publication

visionnage_jour_pondere <- dataset_tiktok_clean %>%
  group_by(Jour_de_Publication) %>%
  summarise(temps_moyen_pondere = sum(`Temps_Visionnage_Moyen_(s)` * nb_de_Vues) / sum(nb_de_Vues))

print(visionnage_jour_pondere)

ggplot(visionnage_jour_pondere %>% 
         mutate(Jour_de_Publication = factor(Jour_de_Publication, levels = c("LUNDI", "MARDI", "MERCREDI", "JEUDI", "VENDREDI", "SAMEDI", "DIMANCHE"))),
       aes(x = Jour_de_Publication, y = temps_moyen_pondere, fill = Jour_de_Publication)) +
  geom_bar(stat = "identity") +
  labs(title = "Temps de visionnage moyen pondéré selon le jour", x = "Jour", y = "Temps moyen (sec)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set3")



# Type de contenu

visionnage_type_pondere <- dataset_tiktok_clean %>%
  group_by(Type_de_Contenu) %>%
  summarise(temps_moyen_pondere = sum(`Temps_Visionnage_Moyen_(s)` * nb_de_Vues) / sum(nb_de_Vues))

print(visionnage_type_pondere)

ggplot(visionnage_type_pondere, aes(x = Type_de_Contenu, y = temps_moyen_pondere, fill = Type_de_Contenu)) +
  geom_bar(stat = "identity") +
  labs(title = "Temps de visionnage moyen pondéré selon le type de contenu", x = "Type", y = "Temps moyen (sec)") +
  theme_minimal() +
  scale_fill_brewer(palette = "Set1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

