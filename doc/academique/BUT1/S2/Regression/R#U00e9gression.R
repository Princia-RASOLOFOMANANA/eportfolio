rm(list=ls()) 
setwd("C:/Users/ACER/Desktop/BUT1/R/SAE Régression")
.libPaths("C:/Users/ACER/Desktop/BUT1/R/Packages")
load(file = "Data_Projet23.Rdata")
library(MASS)
#install.packages("ascentTraining")
library(ascentTraining)
#install.packages("robusTest")
library (robusTest)
library(dplyr)
summary(Data)
library(skimr)
skim(Data)
library(ggplot2)
library(tidyverse)

'''Pour chaque pays, vous devrez :
- Réaliser une analyse descriptive des variables.
- Effectuer un ajustement polynomial des courbes de croissance, en expliquant la méthode (4 courbes de croissance sont attendues).
- Après avoir divisé la population par tranches d’âge, tracer la courbe de croissance médiane, ainsi que les courbes correspondant aux premier et troisième quartiles.
- Donner les conclusions qui vous semblent les plus marquantes (comparaison filles-garçons, comparaisons entre pays, etc.).'''

#Analyse descriptive des variables
#univarié
#Age
boxplot(Data$Age,main="Répartition de l'age", col = "bisque3")

#Taille
boxplot(Data$Taille,main="Répartition de la taille", col = "azure2")

#bivariée
#Taille Sexe
by(Data$Taille, Data$Sexe, summary)
boxplot(Data$Taille ~ Data$Sexe, col = c("lightblue", "lightgreen"),
        main = "Répartition de la taille selon le sexe",
        xlab = "sexe",
        ylab = "Taille")
#Sexe Age
by(Data$Age, Data$Sexe, summary)
boxplot(Data$Age ~ Data$Sexe, col = c("lightblue", "lightgreen"),
        main = "Répartition de l'age selon le sexe",
        xlab = "Sexe",
        ylab = "Age")
levels(Data$Sexe)

#Taille Pays
by(Data$Taille, Data$Pays, summary)
boxplot(Data$Taille ~ Data$Pays, col = c("bisque", "darkolivegreen3"),
        main = "Répartition de la taille selon le Pays",
        xlab = "Pays",
        ylab = "Taille")

#Age Pays
by(Data$Age, Data$Pays, summary)
boxplot(Data$Age ~ Data$Pays, col = c("bisque", "darkolivegreen3"),
        main = "Répartition de l'age selon le Pays",
        xlab = "Pays",
        ylab = "Age")

#analyse multivariée
#taille sexe pays
Data$Groupe <- with(Data, paste(Pays))
ggplot(Data, aes(x = Groupe, y = Taille, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Répartition de la taille selon le sexe et le pays", x = "Groupe", y = "Taille") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme_minimal()
#age sexe pays
Data$Groupe1 <- with(Data, paste(Pays))
ggplot(Data, aes(x = Groupe, y = Age, fill = Sexe)) +
  geom_boxplot() +
  labs(title = "Répartition de l'age selon le sexe et le pays", x = "Groupe", y = "Age") +
  scale_fill_manual(values = c("skyblue", "lightgreen")) +
  theme_minimal()

#### cas 1: Pays RD Sexe Boys
RDB <- Data %>%
  filter(Pays == "Dominican Republic", Sexe=="Boys")
#### cas 2: pays RD Sexe Girls
RDF <- Data %>%
  filter((Data$Pays == "Dominican Republic")& (Data$Sexe == "Girls"))
#### cas 3: pays SL Sexe Boys
SLB <- Data %>% 
  filter((Data$Pays == "Sri Lanka")& (Data$Sexe == "Boys"))
#### cas 4: pays SL Sexe Girls
SLF <- Data %>% 
  filter((Data$Pays == "Sri Lanka")& (Data$Sexe == "Girls"))

#1. Représentez graphiquement la taille en fonction de l'age
par(mfrow = c(1,1))
#Garcon
plot(Data$Age[Data$Sexe == "Boys"],Data$Taille[Data$Sexe == "Boys"],
     main="Variation de la taille en fonction de l'age des garcons ",
     xlab="age",
     ylab="taille")
#Filles
plot(Data$Age[Data$Sexe == "Girls"],Data$Taille[Data$Sexe == "Girls"],
     main="Variation de la taille en fonction de l'age des filles ",
     xlab="age",
     ylab="taille")
#RD
plot(Data$Age[Data$Pays == "Dominican Republic"],Data$Taille[Data$Pays == "Dominican Republic"],
     main="Variation de la taille en fonction de l'age à Dominican Republic ",
     xlab="age",
     ylab="taille")
#SL
plot(Data$Age[Data$Pays == "Sri Lanka"],Data$Taille[Data$Pays == "Sri Lanka"],
     main="Variation de la taille en fonction de l'age à Sri Lanka ",
     xlab="age",
     ylab="taille")
#RD&Garcon
plot(RDB$Taille~RDB$Age,
     main="Variation de la taille en fonction de l'age chez les garcons au Dominican républic",
     xlab="age",
     ylab="taille")
#RD$Fille
plot(RDF$Taille~RDF$Age,
     main="Variation de la taille en fonction de l'age chez les filles au Dominican républic",
     xlab="age",
     ylab="taille")
#SL$Garcon
plot(SLB$Taille~SLB$Age,
     main="Variation de la taille en fonction de l'age chez les garcons au Sri Lanka",
     xlab="age",
     ylab="taille")
#SL$fille
plot(SLF$Taille~SLF$Age,
     main="Variation de la taille en fonction de l'age chez les filles au Sri Lanka",
     xlab="age",
     ylab="taille")

#paramètres de la droite des moindres carrés et la droite :
#— les coefficients
#coef gars
a <- cov(Data$Age[Data$Sexe == "Boys"], Data$Taille[Data$Sexe == "Boys"]) / var(Data$Age[Data$Sexe == "Boys"])
b <- mean(Data$Taille[Data$Sexe == "Boys"]) - (a*mean(Data$Age[Data$Sexe == "Boys"]))
#coef fille
c <- cov(Data$Age[Data$Sexe == "Girls"], Data$Taille[Data$Sexe == "Girls"]) / var(Data$Age[Data$Sexe == "Girls"])
d <- mean(Data$Taille[Data$Sexe == "Girls"]) - (c*mean(Data$Age[Data$Sexe == "Girls"]))
#coef Dom
e <- cov(Data$Age[Data$Pays == "Dominican Republic"], Data$Taille[Data$Pays == "Dominican Republic"]) / var(Data$Age[Data$Pays == "Dominican Republic"])
f <- mean(Data$Taille[Data$Pays == "Dominican Republic"]) - (e*mean(Data$Age[Data$Pays == "Dominican Republic"]))
#coef sri
g <- cov(Data$Age[Data$Pays == "Sri Lanka"], Data$Taille[Data$Pays == "Sri Lanka"]) / var(Data$Age[Data$Pays == "Sri Lanka"])
h <- mean(Data$Taille[Data$Pays == "Sri Lanka"]) - (g*mean(Data$Age[Data$Pays == "Sri Lanka"]))
#coef RD&gars
i <- cov(RDB$Age, RDB$Taille) / var(RDB$Age)
j <- mean(RDB$Taille) - (i*mean(RDB$Age))
#coef RD&fille
k <- cov(RDF$Age, RDF$Taille) / var(RDF$Age)
l <- mean(RDF$Taille) - (k*mean(RDF$Age))
#coef SL&gars
m <- cov(SLB$Age, SLB$Taille) / var(SLB$Age)
n <- mean(SLB$Taille) - (m*mean(SLB$Age))
#coef SL&fille
o <- cov(SLF$Age, SLF$Taille) / var(SLF$Age)
p <- mean(SLF$Taille) - (o*mean(SLF$Age))

#utilisez la fonction lm de R. La commande summary permet alors d’afficher les coefficients
#des moindres carrés (ainsi que le R2).
model_B <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"])
summary(model_B)
model_F <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"])
summary(model_F)
model_RD <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"])
summary(model_RD)
model_SL <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"])
summary(model_SL)
model_RDB <- lm(data = RDB, Taille~Age)
summary(model_RDB)
model_RDF <- lm(data = RDF, Taille~Age)
summary(model_RDF)
model_SLB <- lm(data = SLB, Taille~Age)
summary(model_SLB)
model_SLF <- lm(data = SLF, Taille~Age)
summary(model_SLF)


# l’ajustement linéaire sur le nuage de points en utilisant la commande abline.
#Garcon
plot(Data$Age[Data$Sexe == "Boys"],Data$Taille[Data$Sexe == "Boys"],
     main="Variation de la taille en fonction de l'age des garcons ",
     xlab="age",
     ylab="taille")
abline(model_B,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#Fille
plot(Data$Age[Data$Sexe == "Girls"],Data$Taille[Data$Sexe == "Girls"],
     main="Variation de la taille en fonction de l'age des filles ",
     xlab="age",
     ylab="taille")
abline(model_F,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#RD
plot(Data$Age[Data$Pays == "Dominican Republic"],Data$Taille[Data$Pays == "Dominican Republic"],
     main="Variation de la taille en fonction de l'age à Dominican Republic ",
     xlab="age",
     ylab="taille")
abline(model_RD,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#SL
plot(Data$Age[Data$Pays == "Sri Lanka"],Data$Taille[Data$Pays == "Sri Lanka"],
     main="Variation de la taille en fonction de l'age à Sri Lanka ",
     xlab="age",
     ylab="taille")
abline(model_SL,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#RD&B
plot(RDB$Age,RDB$Taille,
     main="Variation de la taille en fonction de l'age chez les garcon au Dominican Republic ",
     xlab="age",
     ylab="taille")
abline(model_RDB,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#RD&F
plot(RDF$Age,RDF$Taille,
     main="Variation de la taille en fonction de l'age chez les filles au Dominican Republic ",
     xlab="age",
     ylab="taille")
abline(model_RDF,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#SL&B
plot(SLB$Age,SLB$Taille,
     main="Variation de la taille en fonction de l'age chez les garcon au Sri Lanka ",
     xlab="age",
     ylab="taille")
abline(model_SLB,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)

#SL&F
plot(SLF$Age,SLF$Taille,
     main="Variation de la taille en fonction de l'age chez les filles au Sri lanka ",
     xlab="age",
     ylab="taille")
abline(model_SLF,col="red", lwd = 2)
legend("topleft", "Ajustement linéaire", col = "red", lty = 1, lwd = 2)


#3.  le coefficient de corrélation ainsi que le R2
cor(Data$Age[Data$Sexe == "Boys"],Data$Taille[Data$Sexe == "Boys"])
cor(Data$Age[Data$Sexe == "Boys"],Data$Taille[Data$Sexe == "Boys"])^2

cor(Data$Age[Data$Sexe == "Girls"],Data$Taille[Data$Sexe == "Girls"])
cor(Data$Age[Data$Sexe == "Girls"],Data$Taille[Data$Sexe == "Girls"])^2

cor(Data$Age[Data$Pays == "Dominican Republic"],Data$Taille[Data$Pays == "Dominican Republic"])
cor(Data$Age[Data$Pays == "Dominican Republic"],Data$Taille[Data$Pays == "Dominican Republic"])^2

cor(Data$Age[Data$Pays == "Sri Lanka"],Data$Taille[Data$Pays == "Sri Lanka"])
cor(Data$Age[Data$Pays == "Sri Lanka"],Data$Taille[Data$Pays == "Sri Lanka"])^2

cor(RDB$Age,RDB$Taille)
cor(RDB$Age,RDB$Taille)^2

cor(RDF$Age,RDF$Taille)
cor(RDF$Age,RDF$Taille)^2

cor(SLB$Age,SLB$Taille)
cor(SLB$Age,SLB$Taille)^2

cor(SLF$Age,SLF$Taille)
cor(SLF$Age,SLF$Taille)^2

#les valeurs ajustées du modèle avec la commande fitted.
b_hat <- fitted(model_B)
f_hat <- fitted(model_F)
rd_hat <- fitted(model_RD)
sl_hat <- fitted(model_SL)

rdb_hat <- fitted(model_RDB)
rdf_hat <- fitted(model_RDF)
slb_hat <- fitted(model_SLB)
slf_hat <- fitted(model_SLF)

#5. les résidus du modèle ainsi que leur écart-type. l’histogramme des résidus, la boîte à moustache des résidus, puis représentez le nuage de points des résidus en
#fonction de X (avec les droites horizontales à +/- deux fois l’écart-type estimé). Commentez
#l’ajustement.
residuB <- model_B$residuals
ecartB <- sd(residuB)
residuF <- model_F$residuals
ecartF <- sd(residuF)
residuRD <- model_RD$residuals
ecartRD <- sd(residuRD)
residuSL <- model_SL$residuals
ecartSL <- sd(residuSL)

residuRDB <- model_RDB$residuals
ecartRDB <- sd(residuRDB)
residuRDF <- model_RDF$residuals
ecartRDF <- sd(residuRDF)
residuSLB <- model_SLB$residuals
ecartSLB <- sd(residuSLB)
residuSLF <- model_SLF$residuals
ecartSLF <- sd(residuSLF)

hist(residuB,main="Répartition des résidus des garcons",freq = FALSE)
boxplot(residuB,main="Résidus des garcons")
hist(residuF,main="Répartition des résidus des filles",freq = FALSE)
boxplot(residuF,main="Résidus des filles")
hist(residuRD,main="Répartition des résidus au Dominican Republic",freq = FALSE)
boxplot(residuRD,main="Résidus au Dominican Republic")
hist(residuSL,main="Répartition des résidus au Sri Lanka",freq = FALSE)
boxplot(residuSL,main="Résidus au Sri Lanka")

hist(residuRDB,main="Répartition des résidus des garcons au Dominican Republic",freq = FALSE)
boxplot(residuRDB,main="Résidus des garcons au Dominican Republic")
hist(residuRDF,main="Répartition des résidus des filles au Dominican Republic",freq = FALSE)
boxplot(residuRDF,main="Résidus des filles au Dominican Republic")
hist(residuSLB,main="Répartition des résidus des garcons au Sri Lanka",freq = FALSE)
boxplot(residuSLB,main="Résidus des garcons au Sri Lanka")
hist(residuSLF,main="Répartition des résidus des filles au Sri Lanka",freq = FALSE)
boxplot(residuSLF,main="Résidus des filles au Sri Lanka")

plot(Data$Age[Data$Sexe == "Boys"],residuB,
     xlab="Age",
     ylab="Residus des garcons")
abline(h=0, lwd = 2)
abline(h=2*ecartB,col="red",lty=2, lwd = 2)
abline(h=-2*ecartB,col="red",lty=2, lwd = 2)

plot(Data$Age[Data$Sexe == "Girls"],residuF,
     xlab="Age",
     ylab="Residus des filles")
abline(h=0, lwd = 2)
abline(h=2*ecartF,col="red",lty=2, lwd = 2)
abline(h=-2*ecartF,col="red",lty=2, lwd = 2)

plot(Data$Age[Data$Pays == "Dominican Republic"],residuRD,
     xlab="Age",
     ylab="Residus au Dominican Republic")
abline(h=0, lwd = 2)
abline(h=2*ecartRD,col="red",lty=2, lwd = 2)
abline(h=-2*ecartRD,col="red",lty=2, lwd = 2)

plot(Data$Age[Data$Pays == "Sri Lanka"],residuSL,
     xlab="Age",
     ylab="Residus au Sri Lanka")
abline(h=0, lwd = 2)
abline(h=2*ecartSL,col="red",lty=2, lwd = 2)
abline(h=-2*ecartSL,col="red",lty=2, lwd = 2)

plot(RDB$Age,residuRDB,
     xlab="Age",
     ylab="Residus des garcons au Dominican Republic")
abline(h=0, lwd = 2)
abline(h=2*ecartRDB,col="red",lty=2, lwd = 2)
abline(h=-2*ecartRDB,col="red",lty=2, lwd = 2)

plot(RDF$Age,residuRDF,
     xlab="Age",
     ylab="Residus des filles au Dominican Republic")
abline(h=0, lwd = 2)
abline(h=2*ecartRDF,col="red",lty=2, lwd = 2)
abline(h=-2*ecartRDF,col="red",lty=2, lwd = 2)

plot(SLB$Age,residuSLB,
     xlab="Age",
     ylab="Residus des garcons au Sri Lanka")
abline(h=0, lwd = 2)
abline(h=2*ecartSLB,col="red",lty=2, lwd = 2)
abline(h=-2*ecartSLB,col="red",lty=2, lwd = 2)

plot(SLF$Age,residuSLF,
     xlab="Age",
     ylab="Residus des filles au Sri Lanka")
abline(h=0, lwd = 2)
abline(h=2*ecartSLF,col="red",lty=2, lwd = 2)
abline(h=-2*ecartSLF,col="red",lty=2, lwd = 2)

#Pour une nouvelle valeur de X, donnez une prévision de Y à l’aide de la commande predict.
newdataB=data.frame(Age=3)
predict(model_B, newdataB,interval="prediction")
newdataF=data.frame(Age=3)
predict(model_F, newdataF,interval="prediction")
newdataRD=data.frame(Age=3)
predict(model_RD, newdataRD,interval="prediction")
newdataSL=data.frame(Age=3)
predict(model_SL, newdataSL,interval="prediction")

newdataRDB=data.frame(Age=3)
predict(model_RDB, newdataRDB,interval="prediction")
newdataRDF=data.frame(Age=3)
predict(model_RDF, newdataRDF,interval="prediction")
newdataSLB=data.frame(Age=3)
predict(model_SLB, newdataSLB,interval="prediction")
newdataSLF=data.frame(Age=3)
predict(model_SLF, newdataSLF,interval="prediction")

#7. Ajustez Y en fonction de X par un polynôme d’ordre 2. Donnez le R2 à l’aide de la commande
#summary. Mêmes questions pour des polynômes d’ordres supérieurs. Tracez ces polynômes sur
#le nuage de points et commentez.
#garcon
model_B2 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2))
summary(model_B2)

model_B3 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2)+I(Age[Data$Sexe == "Boys"]^3))
summary(model_B3)
model_B4 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2)+I(Age[Data$Sexe == "Boys"]^3)+ I(Age[Data$Sexe == "Boys"]^4))
summary(model_B4)
model_B5 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2)+I(Age[Data$Sexe == "Boys"]^3)+ I(Age[Data$Sexe == "Boys"]^4)+ I(Age[Data$Sexe == "Boys"]^5))
summary(model_B5)
model_B6 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2)+I(Age[Data$Sexe == "Boys"]^3)+ I(Age[Data$Sexe == "Boys"]^4)+ I(Age[Data$Sexe == "Boys"]^5)+ I(Age[Data$Sexe == "Boys"]^6))
summary(model_B6)
model_B7 <- lm(data = Data, Taille[Data$Sexe == "Boys"]~Age[Data$Sexe == "Boys"]+I(Age[Data$Sexe == "Boys"]^2)+I(Age[Data$Sexe == "Boys"]^3)+ I(Age[Data$Sexe == "Boys"]^4)+ I(Age[Data$Sexe == "Boys"]^5)+ I(Age[Data$Sexe == "Boys"]^6)+ I(Age[Data$Sexe == "Boys"]^7))
summary(model_B7)

#Garcon
AIC(model_B)
AIC(model_B2)
AIC(model_B3)
AIC(model_B4)
AIC(model_B5)
AIC(model_B6)
AIC(model_B7)

plot(Data$Age[Data$Sexe == "Boys"],
     Data$Taille[Data$Sexe == "Boys"],
     main="Variation de la taille des garcons en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_B,col="red", lwd = 2)
x = seq(min(Data$Age[Data$Sexe == "Boys"]), max(Data$Age[Data$Sexe == "Boys"]), by = 0.5)

polB2 = coefficients(model_B2)[1] + coefficients(model_B2)[2]*x + coefficients(model_B2)[3]*x^2
lines(x, polB2, col = "blue", lwd = 2)

polB3 = coefficients(model_B3)[1] + coefficients(model_B3)[2]*x + coefficients(model_B3)[3]*x^2 + coefficients(model_B3)[4]*x^3
lines(x, polB3, col = "green", lwd = 2)

polB4 = coefficients(model_B4)[1] + coefficients(model_B4)[2]*x + coefficients(model_B4)[3]*x^2 + coefficients(model_B4)[4]*x^3 + coefficients(model_B4)[5]*x^4
lines(x, polB4, col = "pink", lwd = 2)

polB5 = coefficients(model_B5)[1] + coefficients(model_B5)[2]*x + coefficients(model_B5)[3]*x^2 + coefficients(model_B5)[4]*x^3 + coefficients(model_B5)[5]*x^4 + coefficients(model_B5)[6]*x^5
lines(x, polB5, col = "brown", lwd = 2)

polB6 = coefficients(model_B6)[1] + coefficients(model_B6)[2]*x + coefficients(model_B6)[3]*x^2 + coefficients(model_B6)[4]*x^3 + coefficients(model_B6)[5]*x^4 + coefficients(model_B6)[6]*x^5 + coefficients(model_B6)[7]*x^6
lines(x, polB6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

#Fille
model_F2 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2))
summary(model_F2)

model_F3 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2)+I(Age[Data$Sexe == "Girls"]^3))
summary(model_F3)
model_F4 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2)+I(Age[Data$Sexe == "Girls"]^3)+ I(Age[Data$Sexe == "Girls"]^4))
summary(model_F4)
model_F5 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2)+I(Age[Data$Sexe == "Girls"]^3)+ I(Age[Data$Sexe == "Girls"]^4)+ I(Age[Data$Sexe == "Girls"]^5))
summary(model_F5)
model_F6 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2)+I(Age[Data$Sexe == "Girls"]^3)+ I(Age[Data$Sexe == "Girls"]^4)+ I(Age[Data$Sexe == "Girls"]^5)+ I(Age[Data$Sexe == "Girls"]^6))
summary(model_F6)
model_F7 <- lm(data = Data, Taille[Data$Sexe == "Girls"]~Age[Data$Sexe == "Girls"]+I(Age[Data$Sexe == "Girls"]^2)+I(Age[Data$Sexe == "Girls"]^3)+ I(Age[Data$Sexe == "Girls"]^4)+ I(Age[Data$Sexe == "Girls"]^5)+ I(Age[Data$Sexe == "Girls"]^6)+ I(Age[Data$Sexe == "Girls"]^7))
summary(model_F7)

#Fille
AIC(model_F)
AIC(model_F2)
AIC(model_F3)
AIC(model_F4)
AIC(model_F5)
AIC(model_F6)
AIC(model_F7)


plot(Data$Age[Data$Sexe == "Girls"],
     Data$Taille[Data$Sexe == "Girls"],
     main="Variation de la taille des filles en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_F,col="red", lwd = 2)
x = seq(min(Data$Age[Data$Sexe == "Girls"]), max(Data$Age[Data$Sexe == "Girls"]), by = 0.5)

polF2 = coefficients(model_F2)[1] + coefficients(model_F2)[2]*x + coefficients(model_F2)[3]*x^2
lines(x, polF2, col = "blue", lwd = 2)

polF3 = coefficients(model_F3)[1] + coefficients(model_F3)[2]*x + coefficients(model_F3)[3]*x^2 + coefficients(model_F3)[4]*x^3
lines(x, polF3, col = "green", lwd = 2)

polF4 = coefficients(model_F4)[1] + coefficients(model_F4)[2]*x + coefficients(model_F4)[3]*x^2 + coefficients(model_F4)[4]*x^3 + coefficients(model_F4)[5]*x^4
lines(x, polF4, col = "pink", lwd = 2)

polF5 = coefficients(model_F5)[1] + coefficients(model_F5)[2]*x + coefficients(model_F5)[3]*x^2 + coefficients(model_F5)[4]*x^3 + coefficients(model_F5)[5]*x^4+ coefficients(model_F5)[6]*x^5
lines(x, polF5, col = "brown", lwd = 2)

polF6 = coefficients(model_F6)[1] + coefficients(model_F6)[2]*x + coefficients(model_F6)[3]*x^2 + coefficients(model_F6)[4]*x^3 + coefficients(model_F6)[5]*x^4+ coefficients(model_F6)[6]*x^5+ coefficients(model_F6)[7]*x^6
lines(x, polF6, col = "orange", lwd = 2)

legend("topleft",title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

#RD
model_RD2 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2))
summary(model_RD2)

model_RD3 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2)+I(Age[Data$Pays == "Dominican Republic"]^3))
summary(model_RD3)
model_RD4 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2)+I(Age[Data$Pays == "Dominican Republic"]^3)+ I(Age[Data$Pays == "Dominican Republic"]^4))
summary(model_RD4)
model_RD5 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2)+I(Age[Data$Pays == "Dominican Republic"]^3)+ I(Age[Data$Pays == "Dominican Republic"]^4)+ I(Age[Data$Pays == "Dominican Republic"]^5))
summary(model_RD5)
model_RD6 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2)+I(Age[Data$Pays == "Dominican Republic"]^3)+ I(Age[Data$Pays == "Dominican Republic"]^4)+ I(Age[Data$Pays == "Dominican Republic"]^5)+ I(Age[Data$Pays == "Dominican Republic"]^6))
summary(model_RD6)
model_RD7 <- lm(data = Data, Taille[Data$Pays == "Dominican Republic"]~Age[Data$Pays == "Dominican Republic"]+I(Age[Data$Pays == "Dominican Republic"]^2)+I(Age[Data$Pays == "Dominican Republic"]^3)+ I(Age[Data$Pays == "Dominican Republic"]^4)+ I(Age[Data$Pays == "Dominican Republic"]^5)+ I(Age[Data$Pays == "Dominican Republic"]^6)+ I(Age[Data$Pays == "Dominican Republic"]^7))
summary(model_RD7)


#Dominican Republic
AIC(model_RD)
AIC(model_RD2)
AIC(model_RD3)
AIC(model_RD4)
AIC(model_RD5)
AIC(model_RD6)
AIC(model_RD7)

plot(Data$Age[Data$Pays == "Dominican Republic"],
     Data$Taille[Data$Pays == "Dominican Republic"],
     main="Variation de la taille au Dominican Republic en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_RD,col="red", lwd = 2)
x = seq(min(Data$Age[Data$Pays == "Dominican Republic"]), max(Data$Age[Data$Pays == "Dominican Republic"]), by = 0.5)

polRD2 = coefficients(model_RD2)[1] + coefficients(model_RD2)[2]*x + coefficients(model_RD2)[3]*x^2
lines(x, polRD2, col = "blue", lwd = 2)

polRD3 = coefficients(model_RD3)[1] + coefficients(model_RD3)[2]*x + coefficients(model_RD3)[3]*x^2 + coefficients(model_RD3)[4]*x^3
lines(x, polRD3, col = "green", lwd = 2)

polRD4 = coefficients(model_RD4)[1] + coefficients(model_RD4)[2]*x + coefficients(model_RD4)[3]*x^2 + coefficients(model_RD4)[4]*x^3 + coefficients(model_RD4)[5]*x^4
lines(x, polRD4, col = "pink", lwd = 2)

polRD5 = coefficients(model_RD5)[1] + coefficients(model_RD5)[2]*x + coefficients(model_RD5)[3]*x^2 + coefficients(model_RD5)[4]*x^3 + coefficients(model_RD5)[5]*x^4+ coefficients(model_RD5)[6]*x^5
lines(x, polRD5, col = "brown", lwd = 2)

polRD6 = coefficients(model_RD6)[1] + coefficients(model_RD6)[2]*x + coefficients(model_RD6)[3]*x^2 + coefficients(model_RD6)[4]*x^3 + coefficients(model_RD6)[5]*x^4+ coefficients(model_RD6)[6]*x^5 + coefficients(model_RD6)[7]*x^6
lines(x, polRD6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

#Sri Lanka
model_SL2 <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"]+I(Age[Data$Pays == "Sri Lanka"]^2))
summary(model_SL2)

model_SL3 <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"]+I(Age[Data$Pays == "Sri Lanka"]^2)+I(Age[Data$Pays == "Sri Lanka"]^3))
summary(model_SL3)
model_SL4 <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"]+I(Age[Data$Pays == "Sri Lanka"]^2)+I(Age[Data$Pays == "Sri Lanka"]^3)+ I(Age[Data$Pays == "Sri Lanka"]^4))
summary(model_SL4)
model_SL5 <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"]+I(Age[Data$Pays == "Sri Lanka"]^2)+I(Age[Data$Pays == "Sri Lanka"]^3)+ I(Age[Data$Pays == "Sri Lanka"]^4)+ I(Age[Data$Pays == "Sri Lanka"]^5))
summary(model_SL5)
model_SL6 <- lm(data = Data, Taille[Data$Pays == "Sri Lanka"]~Age[Data$Pays == "Sri Lanka"]+I(Age[Data$Pays == "Sri Lanka"]^2)+I(Age[Data$Pays == "Sri Lanka"]^3)+ I(Age[Data$Pays == "Sri Lanka"]^4)+ I(Age[Data$Pays == "Sri Lanka"]^5)+ I(Age[Data$Pays == "Sri Lanka"]^6))
summary(model_SL6)


#Sri Lanka
AIC(model_SL)
AIC(model_SL2)
AIC(model_SL3)
AIC(model_SL4)
AIC(model_SL5)
AIC(model_SL6)



plot(Data$Age[Data$Pays == "Sri Lanka"],
     Data$Taille[Data$Pays == "Sri Lanka"],
     main="Variation de la taille au Sri Lanka en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_SL,col="red", lwd = 2)
x = seq(min(Data$Age[Data$Pays == "Sri Lanka"]), max(Data$Age[Data$Pays == "Sri Lanka"]), by = 0.5)

polSL2 = coefficients(model_SL2)[1] + coefficients(model_SL2)[2]*x + coefficients(model_SL2)[3]*x^2
lines(x, polSL2, col = "blue", lwd = 2)

polSL3 = coefficients(model_SL3)[1] + coefficients(model_SL3)[2]*x + coefficients(model_SL3)[3]*x^2 + coefficients(model_SL3)[4]*x^3
lines(x, polSL3, col = "green", lwd = 2)

polSL4 = coefficients(model_SL4)[1] + coefficients(model_SL4)[2]*x + coefficients(model_SL4)[3]*x^2 + coefficients(model_SL4)[4]*x^3 + coefficients(model_SL4)[5]*x^4
lines(x, polSL4, col = "pink", lwd = 2)

polSL5 = coefficients(model_SL5)[1] + coefficients(model_SL5)[2]*x + coefficients(model_SL5)[3]*x^2 + coefficients(model_SL5)[4]*x^3 + coefficients(model_SL5)[5]*x^4+ coefficients(model_SL5)[6]*x^5
lines(x, polSL5, col = "brown", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5"),
       col = c("red", "blue", "green", "pink", "brown"),
       lwd = 2)

#RDB
model_RDB2 <- lm(data = RDB, Taille~Age+I(Age^2))
summary(model_RDB2)

model_RDB3 <- lm(data = RDB, Taille~Age+I(Age^2)+I(Age^3))
summary(model_RDB3)
model_RDB4 <- lm(data = RDB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4))
summary(model_RDB4)
model_RDB5 <- lm(data = RDB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5))
summary(model_RDB5)
model_RDB6 <- lm(data = RDB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6))
summary(model_RDB6)
model_RDB7 <- lm(data = RDB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6)+ I(Age^7))
summary(model_RDB7)

#RDB
AIC(model_RDB)
AIC(model_RDB2)
AIC(model_RDB3)
AIC(model_RDB4)
AIC(model_RDB5)
AIC(model_RDB6)
AIC(model_RDB7)


plot(RDB$Age,
     RDB$Taille,
     main="Variation de la taille des garcons au Dominican Republic en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_RDB,col="red", lwd = 2)
x = seq(min(RDB$Age), max(RDB$Age), by = 0.5)

polRDB2 = coefficients(model_RDB2)[1] + coefficients(model_RDB2)[2]*x + coefficients(model_RDB2)[3]*x^2
lines(x, polRDB2, col = "blue", lwd = 2)

polRDB3 = coefficients(model_RDB3)[1] + coefficients(model_RDB3)[2]*x + coefficients(model_RDB3)[3]*x^2 + coefficients(model_RDB3)[4]*x^3
lines(x, polRDB3, col = "green", lwd = 2)

polRDB4 = coefficients(model_RDB4)[1] + coefficients(model_RDB4)[2]*x + coefficients(model_RDB4)[3]*x^2 + coefficients(model_RDB4)[4]*x^3 + coefficients(model_RDB4)[5]*x^4
lines(x, polRDB4, col = "pink", lwd = 2)

polRDB5 = coefficients(model_RDB5)[1] + coefficients(model_RDB5)[2]*x + coefficients(model_RDB5)[3]*x^2 + coefficients(model_RDB5)[4]*x^3 + coefficients(model_RDB5)[5]*x^4 + coefficients(model_RDB5)[6]*x^5
lines(x, polRDB5, col = "brown", lwd = 2)

polRDB6 = coefficients(model_RDB6)[1] + coefficients(model_RDB6)[2]*x + coefficients(model_RDB6)[3]*x^2 + coefficients(model_RDB6)[4]*x^3 + coefficients(model_RDB6)[5]*x^4 + coefficients(model_RDB6)[6]*x^5 + coefficients(model_RDB6)[7]*x^6
lines(x, polRDB6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

#RDF
model_RDF2 <- lm(data = RDF, Taille~Age+I(Age^2))
summary(model_RDF2)

model_RDF3 <- lm(data = RDF, Taille~Age+I(Age^2)+I(Age^3))
summary(model_RDF3)
model_RDF4 <- lm(data = RDF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4))
summary(model_RDF4)
model_RDF5 <- lm(data = RDF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5))
summary(model_RDF5)
model_RDF6 <- lm(data = RDF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6))
summary(model_RDF6)
model_RDF7 <- lm(data = RDF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6)+ I(Age^7))
summary(model_RDF7)

#RDF
AIC(model_RDF)
AIC(model_RDF2)
AIC(model_RDF3)
AIC(model_RDF4)
AIC(model_RDF5)
AIC(model_RDF6)
AIC(model_RDF7)

plot(RDF$Age,
     RDF$Taille,
     main="Variation de la taille des filles au Dominican Republic en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_RDF,col="red", lwd = 2)
x = seq(min(RDF$Age), max(RDF$Age), by = 0.5)

polRDF2 = coefficients(model_RDF2)[1] + coefficients(model_RDF2)[2]*x + coefficients(model_RDF2)[3]*x^2
lines(x, polRDF2, col = "blue", lwd = 2)

polRDF3 = coefficients(model_RDF3)[1] + coefficients(model_RDF3)[2]*x + coefficients(model_RDF3)[3]*x^2 + coefficients(model_RDF3)[4]*x^3
lines(x, polRDF3, col = "green", lwd = 2)

polRDF4 = coefficients(model_RDF4)[1] + coefficients(model_RDF4)[2]*x + coefficients(model_RDF4)[3]*x^2 + coefficients(model_RDF4)[4]*x^3 + coefficients(model_RDF4)[5]*x^4
lines(x, polRDF4, col = "pink", lwd = 2)

polRDF5 = coefficients(model_RDF5)[1] + coefficients(model_RDF5)[2]*x + coefficients(model_RDF5)[3]*x^2 + coefficients(model_RDF5)[4]*x^3 + coefficients(model_RDF5)[5]*x^4 + coefficients(model_RDF5)[6]*x^5
lines(x, polRDF5, col = "brown", lwd = 2)

polRDF6 = coefficients(model_RDF6)[1] + coefficients(model_RDF6)[2]*x + coefficients(model_RDF6)[3]*x^2 + coefficients(model_RDF6)[4]*x^3 + coefficients(model_RDF6)[5]*x^4 + coefficients(model_RDF6)[6]*x^5 + coefficients(model_RDF6)[7]*x^6
lines(x, polRDF6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)


#SLB
model_SLB2 <- lm(data = SLB, Taille~Age+I(Age^2))
summary(model_SLB2)

model_SLB3 <- lm(data = SLB, Taille~Age+I(Age^2)+I(Age^3))
summary(model_SLB3)
model_SLB4 <- lm(data = SLB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4))
summary(model_SLB4)
model_SLB5 <- lm(data = SLB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5))
summary(model_SLB5)
model_SLB6 <- lm(data = SLB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6))
summary(model_SLB6)
model_SLB7 <- lm(data = SLB, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6)+ I(Age^7))
summary(model_SLB7)

#SLB
AIC(model_SLB)
AIC(model_SLB2)
AIC(model_SLB3)
AIC(model_SLB4)
AIC(model_SLB5)
AIC(model_SLB6)
AIC(model_SLB7)

plot(SLB$Age,
     SLB$Taille,
     main="Variation de la taille des garcons au Sri Lanka en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_SLB,col="red", lwd = 2)
x = seq(min(SLB$Age), max(SLB$Age), by = 0.5)

polSLB2 = coefficients(model_SLB2)[1] + coefficients(model_SLB2)[2]*x + coefficients(model_SLB2)[3]*x^2
lines(x, polSLB2, col = "blue", lwd = 2)

polSLB3 = coefficients(model_SLB3)[1] + coefficients(model_SLB3)[2]*x + coefficients(model_SLB3)[3]*x^2 + coefficients(model_SLB3)[4]*x^3
lines(x, polSLB3, col = "green", lwd = 2)

polSLB4 = coefficients(model_SLB4)[1] + coefficients(model_SLB4)[2]*x + coefficients(model_SLB4)[3]*x^2 + coefficients(model_SLB4)[4]*x^3 + coefficients(model_SLB4)[5]*x^4
lines(x, polSLB4, col = "pink", lwd = 2)

polSLB5 = coefficients(model_SLB5)[1] + coefficients(model_SLB5)[2]*x + coefficients(model_SLB5)[3]*x^2 + coefficients(model_SLB5)[4]*x^3 + coefficients(model_SLB5)[5]*x^4 + coefficients(model_SLB5)[6]*x^5
lines(x, polSLB5, col = "brown", lwd = 2)

polSLB6 = coefficients(model_SLB6)[1] + coefficients(model_SLB6)[2]*x + coefficients(model_SLB6)[3]*x^2 + coefficients(model_SLB6)[4]*x^3 + coefficients(model_SLB6)[5]*x^4 + coefficients(model_SLB6)[6]*x^5+ coefficients(model_SLB6)[7]*x^6
lines(x, polSLB6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

#SLF
model_SLF2 <- lm(data = SLF, Taille~Age+I(Age^2))
summary(model_SLF2)

model_SLF3 <- lm(data = SLF, Taille~Age+I(Age^2)+I(Age^3))
summary(model_SLF3)
model_SLF4 <- lm(data = SLF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4))
summary(model_SLF4)
model_SLF5 <- lm(data = SLF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5))
summary(model_SLF5)
model_SLF6 <- lm(data = SLF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6))
summary(model_SLF6)
model_SLF7 <- lm(data = SLF, Taille~Age+I(Age^2)+I(Age^3)+ I(Age^4)+ I(Age^5)+ I(Age^6)+ I(Age^7))
summary(model_SLF7)

#SLF
AIC(model_SLF)
AIC(model_SLF2)
AIC(model_SLF3)
AIC(model_SLF4)
AIC(model_SLF5)
AIC(model_SLF6)
AIC(model_SLF7)

plot(SLF$Age,
     SLF$Taille,
     main="Variation de la taille des filles au Sri Lanka en fonction de leur age",
     xlab="Age",
     ylab="Taille")
abline(model_SLF,col="red", lwd = 2)
x = seq(min(SLF$Age), max(SLF$Age), by = 0.5)

polSLF2 = coefficients(model_SLF2)[1] + coefficients(model_SLF2)[2]*x + coefficients(model_SLF2)[3]*x^2
lines(x, polSLF2, col = "blue", lwd = 2)

polSLF3 = coefficients(model_SLF3)[1] + coefficients(model_SLF3)[2]*x + coefficients(model_SLF3)[3]*x^2 + coefficients(model_SLF3)[4]*x^3
lines(x, polSLF3, col = "green", lwd = 2)

polSLF4 = coefficients(model_SLF4)[1] + coefficients(model_SLF4)[2]*x + coefficients(model_SLF4)[3]*x^2 + coefficients(model_SLF4)[4]*x^3 + coefficients(model_SLF4)[5]*x^4
lines(x, polSLF4, col = "pink", lwd = 2)

polSLF5 = coefficients(model_SLF5)[1] + coefficients(model_SLF5)[2]*x + coefficients(model_SLF5)[3]*x^2 + coefficients(model_SLF5)[4]*x^3 + coefficients(model_SLF5)[5]*x^4 + coefficients(model_SLF5)[6]*x^5
lines(x, polSLF5, col = "brown", lwd = 2)

polSLF6 = coefficients(model_SLF6)[1] + coefficients(model_SLF6)[2]*x + coefficients(model_SLF6)[3]*x^2 + coefficients(model_SLF6)[4]*x^3 + coefficients(model_SLF6)[5]*x^4 + coefficients(model_SLF6)[6]*x^5 + coefficients(model_SLF6)[7]*x^6
lines(x, polSLF6, col = "orange", lwd = 2)

legend("topleft", title = "Ajustement polynomial",
       legend = c("Linéaire (deg 1)", "Polynôme deg 2", "Polynôme deg 3", "Polynôme deg 4", "Polynôme deg 5", "Polynôme deg 6"),
       col = c("red", "blue", "green", "pink", "brown", "orange"),
       lwd = 2)

##
newx <- cut(Data$Age, breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
#global
newy <- tapply(Data$Taille, newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(Data$Taille, newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(Data$Taille, newx, quar3)

par(mfrow=c(1,1))
plot(Data$Age, Data$Taille, main = "Distribution globale par age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))

#Garcon
newx <- cut(Data$Age[Data$Sexe == "Boys"], breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(Data$Taille[Data$Sexe == "Boys"], newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(Data$Taille[Data$Sexe == "Boys"], newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(Data$Taille[Data$Sexe == "Boys"], newx, quar3)

par(mfrow=c(1,1))
plot(Data$Age[Data$Sexe == "Boys"], Data$Taille[Data$Sexe == "Boys"], main = "Distribution de la taille des garcons par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#Fille
newx <- cut(Data$Age[Data$Sexe == "Girls"], breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(Data$Taille[Data$Sexe == "Girls"], newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(Data$Taille[Data$Sexe == "Girls"], newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(Data$Taille[Data$Sexe == "Girls"], newx, quar3)

par(mfrow=c(1,1))
plot(Data$Age[Data$Sexe == "Girls"], Data$Taille[Data$Sexe == "Girls"], main = "Distribution de la taille des filles par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#RD
newx <- cut(Data$Age[Data$Pays == "Dominican Republic"], breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(Data$Taille[Data$Pays == "Dominican Republic"], newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(Data$Taille[Data$Pays == "Dominican Republic"], newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(Data$Taille[Data$Pays == "Dominican Republic"], newx, quar3)

par(mfrow=c(1,1))
plot(Data$Age[Data$Pays == "Dominican Republic"], Data$Taille[Data$Pays == "Dominican Republic"], main = "Distribution de la taille des habitants du Dominican Republic par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#SL
newx <- cut(Data$Age[Data$Pays == "Sri Lanka"], breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(Data$Taille[Data$Pays == "Sri Lanka"], newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(Data$Taille[Data$Pays == "Sri Lanka"], newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(Data$Taille[Data$Pays == "Sri Lanka"], newx, quar3)

par(mfrow=c(1,1))
plot(Data$Age[Data$Pays == "Sri Lanka"], Data$Taille[Data$Pays == "Sri Lanka"], main = "Distribution de la taille des habitants du Sri Lanka par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#RDB
newx <- cut(RDB$Age, breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(RDB$Taille, newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(RDB$Taille, newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(RDB$Taille, newx, quar3)

par(mfrow=c(1,1))
plot(RDB$Age, RDB$Taille, main = "Distribution de la taille des garcons au Dominican Republic par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#RDF
newx <- cut(RDF$Age, breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(RDF$Taille, newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(RDF$Taille, newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(RDF$Taille, newx, quar3)

par(mfrow=c(1,1))
plot(RDF$Age, RDF$Taille, main = "Distribution de la taille des filles au Dominican Republic par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#SLB
newx <- cut(SLB$Age, breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(SLB$Taille, newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(SLB$Taille, newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(SLB$Taille, newx, quar3)

par(mfrow=c(1,1))
plot(SLB$Age, SLB$Taille, main = "Distribution de la taille des garcons au Sri Lanka par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
#SLF
newx <- cut(SLF$Age, breaks = c(5,6,7,8,9,10,11,12,13,14,15,16,17,18,19))
levels(newx)
table(newx)
newy <- tapply(SLF$Taille, newx, median)
quar1 = function(x){
  quantile(x,0.25)
}

newy2 <- tapply(SLF$Taille, newx, quar1)
quar3 = function(x){
  quantile(x,0.75)
}
newy3 <- tapply(SLF$Taille, newx, quar3)

par(mfrow=c(1,1))
plot(SLF$Age, SLF$Taille, main = "Distribution de la taille des filles au Sri Lanka par classe d'age",
     xlab = "Age",
     ylab = "Taille")
lines(6:19, newy, col=2, lwd=3)
lines(6:19, newy2, col="blue", lwd=2)
lines(6:19, newy3, col="green", lwd=2)

legend("topleft",
       legend = c("1er quartile (Q1)", "Médiane", "3e quartile (Q3)"),
       col = c("blue", "red", "green"),
       lwd = c(2, 3, 2))
