## =================================================================
## SAE : Description et Prévision de séries temporelles
## Données : Production d’électricité solaire USA (2001–2024)
## =================================================================
setwd("Z:/BUT2/Série temporelle/projet")
# 1. Importation des données ---------------------------------------
data <- readLines("CentraleSolaireUSA01-24.txt")  # lecture ligne par ligne
data <- as.numeric(data)                          # conversion en numérique
data <- data[!is.na(data)]
solaire <- ts(data, frequency=12, start=c(2001,1)) 


# Vérification
length(solaire)   # doit faire 24*12 = 288
plot(solaire, main="Production d’électricité solaire (USA, 2001–2024)", 
     ylab="MWh (milliers)", xlab="Année", lwd=1.2) 



# Transformation logarithmique (utile car croissance exponentielle)
solaire.log <- log(solaire+100)
plot(solaire.log, main="Log de la production solaire (USA, 2001–2024)", 
     ylab="log(MWh)", xlab="Année", lwd=1.2)


# 2. Tendance : régression sur moyennes annuelles + moyenne mobile --
y <- as.vector(solaire.log)
x <- as.vector(time(solaire.log))

# Moyennes annuelles
xclass <- cut(x, 24)                 # 24 années
meananual <- tapply(y, xclass, mean)
t <- seq(2001.5, 2024.5, by=1)

plot(solaire.log, main="Tendance par régression sur moyennes annuelles", 
     ylab="log(MWh)", xlab="Année", pch=".")
lines(t, meananual, col="red", lwd=2)
legend("topleft", legend=c("données", "régression annuelle"), 
       text.col=c("black","red"))

# Filtrage par moyenne mobile centrée sur 12 mois (MMC12)
solaire.movac <- filter(solaire.log, filter=c(1/24,rep(1/12,11),1/24))

plot(solaire.log, main="Tendance : comparaison régression et MMC12", 
     ylab="log(MWh)", xlab="Année")
lines(t, meananual, col="red", lwd=2)
lines(x, solaire.movac, col="blue", lwd=2)
legend("topleft", legend=c("données", "régression", "MMC12"), 
       text.col=c("black","red","blue"))


# 3. Décomposition de la série --------------------------------------
dec <- decompose(solaire.log, type="additive")

plot(dec)   # composantes trend, seasonal, random

# Coefficients saisonniers
plot(dec$seasonal, main="Coefficients saisonniers", xlab="Mois", ylab="")

# Résidus
plot(dec$random, main="Résidus", xlab="Année", ylab="")
abline(h=0, col="black")
abline(h=2*sd(dec$random, na.rm=TRUE), col="red", lty=2)
abline(h=-2*sd(dec$random, na.rm=TRUE), col="red", lty=2)

boxplot(dec$random, na.rm=TRUE, col="yellow",
        main="Boîte à moustaches des résidus")


# 4. Série corrigée des variations saisonnières (CVS) ----------------
CVS <- solaire.log - dec$seasonal
plot(CVS, main="Série corrigée des variations saisonnières", ylab="", xlab="Année", lwd=2)


# 5. Prévisions pour 2025 -------------------------------------------

## (a) Méthode 1 : Tendance polynomiale + saison
y.cvs <- as.vector(CVS)
x <- as.vector(time(solaire.log))
reg <- lm(y.cvs ~ x + I(x^2) + I(x^3))   # poly degré 3
summary(reg)

ajust <- predict(reg)
lines(ajust ~ x, col="red")

# Prévision 2025
t.pred <- seq(2025, 2025+11/12, by=1/12)   # 12 mois
y.pred <- predict(reg, newdata=data.frame(x=t.pred))

# Ajouter saisonnalité
saison <- head(dec$seasonal, 12)
prev1 <- y.pred + saison


## (b) Méthode 2 : Holt-Winters
hw <- HoltWinters(solaire.log)
prev2 <- predict(hw, n.ahead=12)


## (c) Méthode 3 : ARIMA
fit.arima <- arima(solaire.log, order=c(2,1,1), 
                   seasonal=list(order=c(1,1,1), period=12))
prev3 <- predict(fit.arima, n.ahead=12)$pred


# Comparaison graphique
plot(solaire.log, main="Prévisions 2025 : comparaison méthodes", 
     ylab="log(MWh)", xlab="Année", xlim=c(2020,2026))
lines(t.pred, prev1, col="purple", lwd=2)
lines(prev2, col="red", lwd=2)
lines(ts(prev3, start=c(2025,1), frequency=12), col="blue", lwd=2)
legend("topleft", legend=c("Trend+Season", "Holt-Winters", "ARIMA"),
       text.col=c("purple","red","blue"))


# 6. Validation sur 2024 --------------------------------------------
solaire.train <- window(solaire.log, end=c(2023,12))
solaire.test  <- window(solaire.log, start=c(2024,1))

# Refaire prévisions sur train
dec.train <- decompose(solaire.train, type="additive")
CVS.train <- solaire.train - dec.train$seasonal
x.train <- as.vector(time(solaire.train))
y.train <- as.vector(CVS.train)
reg.train <- lm(y.train ~ x.train + I(x.train^2) + I(x.train^3))
y.pred2024 <- predict(reg.train, newdata=data.frame(x.train=seq(2024, 2024+11/12, by=1/12)))
prev1.2024 <- y.pred2024 + head(dec.train$seasonal, 12)

hw.train <- HoltWinters(solaire.train)
prev2.2024 <- predict(hw.train, n.ahead=12)

fit.arima.train <- arima(solaire.train, order=c(2,1,1),
                         seasonal=list(order=c(1,1,1), period=12))
prev3.2024 <- predict(fit.arima.train, n.ahead=12)$pred

# MSE
mse1 <- mean((solaire.test - prev1.2024)^2, na.rm=TRUE)
mse2 <- mean((solaire.test - prev2.2024)^2, na.rm=TRUE)
mse3 <- mean((solaire.test - prev3.2024)^2, na.rm=TRUE)

cat("MSE Trend+Season :", mse1, "\n")
cat("MSE Holt-Winters :", mse2, "\n")
cat("MSE ARIMA :", mse3, "\n")








#Prévision par la méthode ARMA 
##Stationnarisation
y=diff(solaire.log,lag=12,differences=1)
x=diff(y,lag=1,differences=1)
plot(solaire.log)
plot(y)
plot(x)
mean(x)

library(tseries)
#install.packages('tseries')
adf.test(solaire.log,alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12)

##Autocorrélation
acf(x) #Q=1, q=1
pacf(x) #P=2, p=3 


##Modèle ARIMA
z1<-arima(solaire.log, order=c(6,1,6),seasonal=list(order=c(2,1,2),period=12))
z1
tsdiag(z1,gof.lag=25)

z2<-arima(solaire.log, order=c(5,1,5),seasonal=list(order=c(2,1,2),period=12))
z2
tsdiag(z2,gof.lag=25)

z3<-arima(solaire.log, order=c(4,1,4),seasonal=list(order=c(1,1,1),period=12))
z3
tsdiag(z3,gof.lag=25)

z4<-arima(solaire.log, order=c(3,1,3),seasonal=list(order=c(0,1,1),period=12))
z4
tsdiag(z4,gof.lag=25)

z5<-arima(solaire.log, order=c(2,1,2),seasonal=list(order=c(0,1,1),period=12))
z5
tsdiag(z5,gof.lag=25)
prev<-predict(z5, n.ahead=25)






##SCRIPT IMPORTANT(FINAL)
###Prévision 2025 

##alternative
library(forecast)
#install.packages("forecast")
dec<-decompose(solaire.log, type="additive")
CVS<-solaire.log-dec$seasonal
plot(CVS)
CVSend<-tail(CVS,n=36)
plot(CVSend)
y=as.vector(CVSend)
x=as.vector(time(CVSend))
reg<-lm(y~x)
summary(reg)
abline(reg$coef)
t<-seq(2025,2025+11/12, by=1/12)
t
length(t)
season<-as.vector(head(dec$seasonal,n=12))
z<-(reg$coefficients[1]+reg$coefficients[2]*t)+season
plot(solaire.log, main="Prévisions 2025 : comparaison méthodes", 
     ylab="log(MWh)", xlab="Année", xlim=c(2020,2026), ylim=c(7,11))

lines(t,z, col="purple", lwd=2)
legend("topleft", legend=c("données", "prévision Trend + season"),
       text.col=c("black",  "purple"), cex=1.2)


##mettre la racine de l'erreur quadratique moyenne

#prev1 <- valeur de la prevision trend season

EQM <- mean((tail(exp(b-100),12)-prev1)^2)

e <- sqrt(EQM)


###prévision Holt-Winters
data.hw<-HoltWinters(solaire.log)
predict(data.hw, n.ahead=12)
#plot(solaire.log, main="Prévision", cex.main=1.2,
     #xlab="année", ylab="taux de CO2", xlim=c(2020,2026),ylim=c(7,11),lwd=2)
lines(predict(data.hw, n.ahead=12), col="red",lwd=2)
legend("topleft", legend=c("données", "prévision Holt Winters"),
       text.col=c("black",  "red"), cex=1.2)

###prévision ARMA
#Stationnarisation
y=diff(solaire.log,lag=12,differences=1)
x=diff(y,lag=1,differences=1)
plot(y)
plot(x)
mean(x)
library(tseries)
adf.test(solaire.log, alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12)


##Autocorrélation
acf(x) #Q=1, q=1
pacf(x) #P=2, p=3 



##Modèle ARIMA
z1<-arima(solaire.log, order=c(6,1,6),seasonal=list(order=c(2,1,2),period=12))
z1
tsdiag(z1,gof.lag=25)

z2<-arima(solaire.log, order=c(5,1,5),seasonal=list(order=c(2,1,2),period=12))
z2
tsdiag(z2,gof.lag=25)

z3<-arima(solaire.log, order=c(4,1,4),seasonal=list(order=c(1,1,1),period=12))
z3
tsdiag(z3,gof.lag=25)

z4<-arima(solaire.log, order=c(3,1,3),seasonal=list(order=c(0,1,1),period=12))
z4
tsdiag(z4,gof.lag=25)

z5<-arima(solaire.log, order=c(2,1,2),seasonal=list(order=c(0,1,1),period=12))
z5
tsdiag(z5,gof.lag=25)
prev<-predict(z5, n.ahead=25)



#prévision
prev<-predict(z5, n.ahead=12)
prev$pred
plot(solaire.log, main="Prévision des productions d'énergie solaire", cex.main=1.2,
     xlab="année", ylab="solaire", xlim=c(2020,2026),ylim=c(7,11),lwd=2)
lines(prev$pred, col="blue",lwd=2)
legend("topleft", legend=c("données", "prévision ARMA"),
       text.col=c("black",  "blue"), cex=1.2)

###comparaison des prévisions
plot(solaire.log, main="Prévision de la production d'énergie solaire en 2025", cex.main=1.2,
     xlab="année", ylab="MWh (milliers)", xlim=c(2020,2026),ylim=c(7,11),lwd=2)
lines(t,z, col="purple", lwd=2)
lines(predict(data.hw, n.ahead=12), col="red",lwd=2)
lines(prev$pred, col="blue",lwd=2)
legend("topleft", legend=c("Alternative", "Holt-Winters", "ARMA"),
       text.col=c("purple",  "red", "blue"), cex=1.2)








####prevision de 2024

##ref de la prévision SAE(Essai)

# Tracer prévisions vs réel pour 2024

b <- ts(solaire.log, frequency=12, start=c(2001,1),end=c(2023,12)) 

plot(b, main="Production d’électricité solaire (USA, 2001–2023)", 
     ylab="MWh (milliers)", xlab="Année", lwd=1.2) 


#Prévision par la méthode ARMA 
##Stationnarisation
y=diff(b,lag=12,differences=1)
x=diff(y,lag=1,differences=1)
plot(b)
plot(y)
plot(x)
mean(x)

#install.packages("tseries")
library(tseries)
adf.test(b,alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12)

##Autocorrélation
acf(x) #Q=1, q=1
pacf(x) #P=2, p=3 


##Modèle ARIMA
z1<-arima(b, order=c(6,1,6),seasonal=list(order=c(2,1,2),period=12))
z1
tsdiag(z1,gof.lag=25)

z2<-arima(b, order=c(5,1,5),seasonal=list(order=c(2,1,2),period=12))
z2
tsdiag(z2,gof.lag=25)

z3<-arima(b, order=c(4,1,4),seasonal=list(order=c(1,1,1),period=12))
z3
tsdiag(z3,gof.lag=25)

z4<-arima(b, order=c(3,1,3),seasonal=list(order=c(0,1,1),period=12))
z4
tsdiag(z4,gof.lag=25)

z5<-arima(b, order=c(2,1,2),seasonal=list(order=c(0,1,1),period=12))
z5
tsdiag(z5,gof.lag=25)
prev1<-predict(z5, n.ahead=12)


#### Tracer prévisions vs réel pour 2024####



##alternative
library(forecast)
#install.packages("forecast")
decb<-decompose(b, type="additive")
CVSb<-b-decb$seasonal
plot(CVSb)
CVSend<-tail(CVSb,n=36)
plot(CVSend)
y=as.vector(CVSend)
x=as.vector(time(CVSend))
reg<-lm(y~x)
summary(reg)
abline(reg$coef)
t<-seq(2024,2024+11/12, by=1/12)
t
length(t)
season<-as.vector(head(decb$seasonal,n=12))
z<-(reg$coefficients[1]+reg$coefficients[2]*t)+season
plot(solaire.log, main="Prévisions 2025 : comparaison méthodes", 
     ylab="log(MWh)", xlab="Année", xlim=c(2020,2025), ylim=c(7,11))

lines(t,z, col="purple", lwd=2)
legend("topleft", legend=c("données", "prévision Trend + season"),
       text.col=c("black",  "purple"), cex=1.2)



###prévision Holt-Winters
data.hw<-HoltWinters(b)
predict(data.hw, n.ahead=12)
plot(b, main="Prévision de la production d'énergie solaire en 2024", cex.main=1.2,
     xlab="année", ylab="MWh (milliers)", xlim=c(2020,2026),ylim=c(7,11),lwd=2)
lines(predict(data.hw, n.ahead=12), col="red",lwd=2)

legend("topleft", legend=c("données", "prévision Holt Winters"),
       text.col=c("black",  "red"), cex=1.2)

###prévision ARMA
#Stationnarisation
y=diff(b,lag=12,differences=1)
x=diff(y,lag=1,differences=1)
plot(y)
plot(x)
mean(x)
library(tseries)
adf.test(b, alternative=c("stationary"),12)
adf.test(x, alternative=c("stationary"),12)


###comparaison des prévisions
plot(solaire.log, main="Prévision de la production d'énergie solaire en 2024", cex.main=1.2,
     xlab="année", ylab="MWh (milliers)", xlim=c(2020,2025),ylim=c(7,11),lwd=2)
lines(t,z, col="purple", lwd=2)
lines(predict(data.hw, n.ahead=12), col="red",lwd=2)
lines(prev$pred, col="blue",lwd=2)
legend("topleft", legend=c("Alternative", "Holt-Winters", "ARMA"),
       text.col=c("purple",  "red", "blue"), cex=1.2)




##mettre la racine de l'erreur quadratique moyenne



prev2 <- predict(data.hw, n.ahead=12)
prev3 <- 

  
EQM <- mean((tail(solaire.log,12)-prev2$pred)^2)





###comparaison des prévisions
plot(exp(solaire.log-100), main="Prévision de la production d'énergie solaire en 2024", cex.main=1.2,
     xlab="année", ylab="MWh (milliers)", xlim=c(2020,2025),lwd=2)
lines(t,exp(z-100), col="purple", lwd=2)
lines(exp(predict(data.hw, n.ahead=12)-100), col="red",lwd=2)
lines(exp(prev$pred-100), col="blue",lwd=2)
legend("topleft", legend=c("Alternative", "Holt-Winters", "ARMA"),
       text.col=c("purple",  "red", "blue"), cex=1.2)
 



#09/01/2026

## --- 1) EQM sur l'échelle LOG (comme dans le cours : variance des résidus) ---

# solaire.log : ts(..., frequency = 12, start = c(2001,1))  # doit inclure 2024 si vous voulez EQM 2024

## Séries d'entraînement et de test en LOG
b_train <- window(solaire.log, start = c(2001,1), end = c(2023,12))
b_test  <- window(solaire.log, start = c(2024,1), end = c(2024,12))  # 12 valeurs réelles 2024

## Trend + Season (log) — votre 'z' est déjà la prévision en log pour 2024
prev1_log <- ts(as.numeric(z), start = c(2024,1), frequency = 12)

## Holt–Winters (log) — ATTENTION: ajuster sur b_train (2001–2023)
data.hw <- HoltWinters(b_train)
prev2_log <- ts(as.numeric(predict(data.hw, n.ahead = 12)),
                start = c(2024,1), frequency = 12)

## ARIMA/ARMA (log) — ajusté sur b_train (ou votre z5 si vous le refaites proprement)
# Exemple si vous avez déjà prev <- predict(z5, n.ahead = 12)
prev3_log <- ts(as.numeric(prev$pred), start = c(2024,1), frequency = 12)

# Résidus de prévision (2024) pour chaque méthode
e1_log <- as.numeric(b_test - prev1_log)
e2_log <- as.numeric(b_test - prev2_log)
e3_log <- as.numeric(b_test - prev3_log)

# Centrer les résidus (conformément au cours)
e1c_log <- e1_log - mean(e1_log, na.rm = TRUE)
e2c_log <- e2_log - mean(e2_log, na.rm = TRUE)
e3c_log <- e3_log - mean(e3_log, na.rm = TRUE)

# EQM = Var(residus) ; ici on prend la moyenne des carrés, conforme au cours
EQM1_log <- mean(e1c_log^2, na.rm = TRUE)
EQM2_log <- mean(e2c_log^2, na.rm = TRUE)
EQM3_log <- mean(e3c_log^2, na.rm = TRUE)

# (Optionnel) RMSE en log
RMSE1_log <- sqrt(EQM1_log)
RMSE2_log <- sqrt(EQM2_log)
RMSE3_log <- sqrt(EQM3_log)

data.frame(
  Methode  = c("Trend+Season", "Holt-Winters", "ARIMA"),
  EQM_log  = c(EQM1_log, EQM2_log, EQM3_log),
  RMSE_log = c(RMSE1_log, RMSE2_log, RMSE3_log)
)
``

#solaire exp

to_orig <- function(x) exp(x-100)

y_test_orig <- to_orig(b_test)
prev1_orig  <- to_orig(prev1_log)
prev2_orig  <- to_orig(prev2_log)
prev3_orig  <- to_orig(prev3_log)

e1 <- as.numeric(y_test_orig - prev1_orig)
e2 <- as.numeric(y_test_orig - prev2_orig)
e3 <- as.numeric(y_test_orig - prev3_orig)

# Centrage comme dans le cours
e1c <- e1 - mean(e1, na.rm = TRUE)
e2c <- e2 - mean(e2, na.rm = TRUE)
e3c <- e3 - mean(e3, na.rm = TRUE)

EQM1 <- mean(e1c^2, na.rm = TRUE); RMSE1 <- sqrt(EQM1)
EQM2 <- mean(e2c^2, na.rm = TRUE); RMSE2 <- sqrt(EQM2)
EQM3 <- mean(e3c^2, na.rm = TRUE); RMSE3 <- sqrt(EQM3)

data.frame(
  Methode      = c("Trend+Season", "Holt-Winters", "ARIMA"),
  EQM_niveau   = c(EQM1, EQM2, EQM3),
  RMSE_niveau  = c(RMSE1, RMSE2, RMSE3)
)




















