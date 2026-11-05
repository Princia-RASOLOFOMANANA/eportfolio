#vider le contenu de la mémoire  
rm(list=ls()) 
setwd("C:/Users/ACER/Desktop/BUT1/SAE/SAE Projet")
load(file = "C:/Users/ACER/Desktop/BUT1/R/Sauvegardes/Base_de_donnees_projet.Rdata")
.libPaths(c("C:/Users/ACER/Desktop/BUT1/R/Packages", .libPaths()))  
library(readr)  
library(dplyr) 
library(skimr) 
library(ggplot2)
View(datafinal)
#STATISTIQUE DESCRIPTIVES RAPIDES
skim(datafinal) 
summary(datafinal)
#Analyse univariée
# DEATH
ggplot(data = datafinal, aes(x = factor(DEATH))) + 
  geom_bar(aes(y = after_stat(prop), group = 1), fill = "cadetblue3") +
  labs(title = "Répartition des patients décédés durant le suivi",
       x = "Patients décédés",
       y = "Fréquence") 
# SWANG1 (Right Heart Catheterization)
ggplot(data = datafinal, aes(x = factor(SWANG1)))+
  geom_bar(aes(y = after_stat(prop),group = 1), fill = "burlywood4") +
  labs(title = "Répartition des patients qui ont subit un catheterization",
       x = "Patients catheterizés",
       y = "Fréquence") 
# Age 
hist(datafinal$AGE, breaks = 40, prob = TRUE,
     main = "Distribution de l'âge des patients",
     xlab = "Âge", ylab = "Densité",
     col = "lightgray", border = "white")
#APS1
datafinal2 <- datafinal %>%
  filter(APS1 >= 0 & APS1 <= 299) %>%
  mutate(APS1_class = case_when(
    APS1 >= 0   & APS1 < 30   ~ "[0–29]",
    APS1 >= 30  & APS1 < 60   ~ "[30–59]",
    APS1 >= 60  & APS1 < 90   ~ "[60–89]",
    APS1 >= 90  & APS1 < 120  ~ "[90–119]",
    APS1 >= 120 & APS1 < 150  ~ "[120–149]",
    APS1 >= 150 & APS1 < 180  ~ "[150–179]",
    APS1 >= 180 & APS1 < 210  ~ "[180–209]",
    APS1 >= 210 & APS1 < 240  ~ "[210–239]",
    APS1 >= 240 & APS1 < 270  ~ "[240–269]",
    APS1 >= 270 & APS1 <= 299 ~ "[270–299]"
  ))
datafinal2$APS1_class <- factor(datafinal2$APS1_class,
                                levels = c("[0–29]", "[30–59]", "[60–89]", "[90–119]",
                                           "[120–149]", "[150–179]", "[180–209]", "[210–239]",
                                           "[240–269]", "[270–299]"))

ggplot(data = datafinal2, aes(x = APS1_class)) +
  geom_bar(aes(y = after_stat(prop), group = 1), fill = "darkolivegreen4") +
  labs(title = "Répartition du score de gravité des patients",
       x = "Score de gravité",
       y = "Fréquence") +
  theme_minimal()
#SCOMA
datafinal1 <- datafinal %>%
  filter(SCOMA1 >= 0 & SCOMA1<= 100 ) %>%
  mutate(SCOMA1_class = case_when(
    SCOMA1 >= 0  & SCOMA1 < 10  ~ "[0–9]",
    SCOMA1 >= 10 & SCOMA1 < 20  ~ "[10–19]",
    SCOMA1 >= 20 & SCOMA1 < 30  ~ "[20–29]",
    SCOMA1 >= 30 & SCOMA1 < 40  ~ "[30–39]",
    SCOMA1 >= 40 & SCOMA1 < 50  ~ "[40–49]",
    SCOMA1 >= 50 & SCOMA1 < 60  ~ "[50–59]",
    SCOMA1 >= 60 & SCOMA1 < 70  ~ "[60–69]",
    SCOMA1 >= 70 & SCOMA1 < 80  ~ "[70–79]",
    SCOMA1 >= 80 & SCOMA1 < 90  ~ "[80–89]",
    SCOMA1 >= 90 & SCOMA1 <= 100 ~ "[90–100]"
  ))
ggplot(data = datafinal1, aes(x = SCOMA1_class)) +
  geom_bar(aes(y = after_stat(prop), group = 1), fill = "burlywood4") +
  labs(title = "Répartition de l'état neurologique des patients",
       x = "Classe de score SCOMA1",
       y = "Fréquence") +
  theme_minimal()
#Fréquence cardiaque
boxplot(datafinal$HRT1, main = "Répartition de la fréquence cardiaque",
        ylab = "BPM", col = "lightcoral")
#Leucocyte(WBLC1)
boxplot(datafinal$WBLC1, main = "Boxplot des leucocytes",
        ylab = "Nombre de leucocytes", col = "wheat")

#ANALYSE AVANCEE
#Analyse bivariée
#Maladie
# Cardiovasculaire
ggplot(data = datafinal, aes(x = factor(CARD), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon la présence de maladie cardiovasculaire",
       x = "Maladie cardiovasculaire (CARD)", y = "Proportion", fill = "Décès") +
  theme_minimal()

# Infectieux
ggplot(data = datafinal, aes(x = factor(SEPS), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon la présence d'infection (SEPS)",
       x = "Infection (SEPS)", y = "Proportion", fill = "Décès") +
  theme_minimal()

# Atteintes antérieures
ggplot(data = datafinal, aes(x = factor(RENALHX), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon insuffisance rénale", x = "Insuffisance rénale", y = "Proportion") +
  theme_minimal()

ggplot(data = datafinal, aes(x = factor(CHRPULHX), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon atteinte pulmonaire", x = "Atteinte pulmonaire", y = "Proportion") +
  theme_minimal()

ggplot(data = datafinal, aes(x = factor(IMMUNHX), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon immunosuppression", x = "Immunosuppression", y = "Proportion") +
  theme_minimal()
#swang
ggplot(data = datafinal, aes(x = factor(SWANG1), fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon mise en place du cathéter de Swan-Ganz",
       x = "SWANG1", y = "Proportion") +
  theme_minimal()
## AGE vs DEATH
levels_age <- c("[18–27]", "[28–37]", "[38–47]", "[48–57]", "[58–67]", "[68–77]", "[78–87]", "[88–97]", "[98–102]")

datafinal5 <- datafinal %>%
  filter(AGE >= 18 & AGE <= 102) %>%
  mutate(AGE_class = case_when(
    AGE >= 18  & AGE <= 27  ~ "[18–27]",
    AGE >= 28  & AGE <= 37  ~ "[28–37]",
    AGE >= 38  & AGE <= 47  ~ "[38–47]",
    AGE >= 48  & AGE <= 57  ~ "[48–57]",
    AGE >= 58  & AGE <= 67  ~ "[58–67]",
    AGE >= 68  & AGE <= 77  ~ "[68–77]",
    AGE >= 78  & AGE <= 87  ~ "[78–87]",
    AGE >= 88  & AGE <= 97  ~ "[88–97]",
    AGE >= 98  & AGE <= 102 ~ "[98–102]",
    TRUE ~ NA_character_
  )) %>%
  mutate(AGE_class = factor(AGE_class, levels = levels_age))
ggplot(data = datafinal5 %>%
         mutate(AGE_class = factor(AGE_class, levels = levels_age)),
       aes(x = AGE_class, fill = factor(DEATH))) + 
  geom_bar(aes(y = after_stat(prop), group = DEATH), position = "fill") +
  labs(x = "Classe d'âge", y = "Proportion", fill = "Décès", 
       title = "Proportion des décès selon la classe d'âge") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# WTKILO1 (poids) vs DEATH
levels_poids <- c("[19–39]", "[40–59]", "[60–79]", "[80–99]", 
                  "[100–119]", "[120–139]", "[140–159]", 
                  "[160–179]", "[180–199]", "[200–210]")
datafinal5 <- datafinal5 %>%
  mutate(
    WTKILO1_class = case_when(
      WTKILO1 >= 19  & WTKILO1 < 40  ~ "[19–39]",
      WTKILO1 >= 40  & WTKILO1 < 60  ~ "[40–59]",
      WTKILO1 >= 60  & WTKILO1 < 80  ~ "[60–79]",
      WTKILO1 >= 80  & WTKILO1 < 100 ~ "[80–99]",
      WTKILO1 >= 100 & WTKILO1 < 120 ~ "[100–119]",
      WTKILO1 >= 120 & WTKILO1 < 140 ~ "[120–139]",
      WTKILO1 >= 140 & WTKILO1 < 160 ~ "[140–159]",
      WTKILO1 >= 160 & WTKILO1 < 180 ~ "[160–179]",
      WTKILO1 >= 180 & WTKILO1 < 200 ~ "[180–199]",
      WTKILO1 >= 200 & WTKILO1 <= 210 ~ "[200–210]",
      TRUE ~ NA_character_
    ),
    WTKILO1_class = factor(WTKILO1_class, levels = levels_poids)
  )
ggplot(data = datafinal5 %>%
         mutate(WTKILO1_class = factor(WTKILO1_class, levels = levels_poids)),
       aes(x = WTKILO1_class, fill = factor(DEATH))) + 
  geom_bar(aes(y = after_stat(prop), group = DEATH), position = "fill") +
  labs(x = "Classe de poids", y = "Proportion", fill = "Décès", 
       title = "Proportion des décès selon la classe de poids") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# APS1 (score gravité) vs DEATH
ggplot(data = datafinal2, aes(x = APS1_class, fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon le score APS1", x = "Classe APS1", y = "Proportion") +
  theme_minimal()

# SCOMA1 (état neurologique) vs DEATH
ggplot(data = datafinal1, aes(x = SCOMA1_class, fill = factor(DEATH))) +
  geom_bar(position = "fill") +
  labs(title = "Décès selon le score neurologique SCOMA1", x = "Classe SCOMA1", y = "Proportion") +
  theme_minimal()


#Test statistiques
#VARIABLES QUALITATIVES
#Chi²
chisq.test(table(datafinal$DEATH, datafinal$CARD))
#Il n’y a pas de lien statistiquement significatif entre la présence d’un diagnostic cardiovasculaire (CARD) et la mortalité 

chisq.test(table(datafinal$DEATH, datafinal$SEPS))
#Les patients atteints d’infection présentent un risque de décès significativement différent.

chisq.test(table(datafinal$DEATH, datafinal$CHRPULHX))
#L’atteinte pulmonaire semble influencer significativement la mortalité.

chisq.test(table(datafinal$DEATH, datafinal$RENALHX))
#L’atteinte rénale ne semble pas liée de manière significative à la mortalité dans cet échantillon.

#fisher
fisher.test(table(datafinal$DEATH, datafinal$IMMUNHX))
#Il existe une association statistiquement significative entre IMMUNHX et la mortalité (DEATH).

#VARIABLES QUANTI QUALI

t.test(APS1 ~ DEATH, data = datafinal)         # score APS
#Le test de Student indique une différence significative du score APS1 entre les patients décédés et non décédés

t.test(SCOMA1 ~ DEATH, data = datafinal)       # score Glasgow
#Une différence très significative est observée sur le score SCOMA1.

t.test(SURV2MD1 ~ DEATH, data = datafinal)     # probabilité survie
#Le test t montre une forte différence significative de la probabilité estimée de survie 

t.test(MEANBP1 ~ DEATH, data = datafinal)      # pression moyenne
#Une différence significative est notée sur la pression moyenne 

t.test(RESP1 ~ DEATH, data = datafinal)        # fréquence respi
#Aucune différence significative n’est observée pour la fréquence respiratoire

t.test(HRT1 ~ DEATH, data = datafinal)         # fréquence cardiaque
#Le test ne montre pas de différence significative 

t.test(WBLC1 ~ DEATH, data = datafinal)        # leucocytes
#Aucune différence significative sur les leucocytes

t.test(AGE ~ DEATH, data = datafinal)
#Le test indique une différence hautement significative

#Modele logistique
datafinal$DEATH_bin <- ifelse(datafinal$DEATH == "Yes", 1, 0)
modele_full <- glm(DEATH_bin ~ AGE + CARD + SEPS + APS1 + SCOMA1, 
                   data = datafinal, 
                   family = binomial)
modele_step <- step(modele_full, direction = "both")
summary(modele_step)
