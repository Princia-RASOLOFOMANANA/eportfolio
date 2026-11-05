setwd("C:/Users/ACER/Desktop/BUT1/R/Estimation")
install.packages(pkgs = "matrixStats", dependencies = TRUE)
library(matrixStats)
require(matrixStats)
.libPaths("C:/Users/ACER/Desktop/BUT1/R/Packages")
library(readxl)
library(tidyverse)
library(ggplot2)
library(skimr)
donnee <- read.table(file = "Star39552_balanced.csv",
                     sep = ",",
                     header = TRUE)
ech30 <- read.table(file = "plx30_1.txt",
                    header = TRUE)
ech100 <- read.table(file = "plx100_1.txt",
                     header = TRUE)



#moyenne théorique
mu = mean(donnee$Plx)
mu

#1-a)
#echantillon 30
ggplot(ech30, aes(y = Plx)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Boxplot de la distance étoile-terre",
    y = "Distance Etoile-Terre")

ggplot(data = ech30) +
  aes(x = Plx) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Répartition de la distance étoile-terre",
       x = "Distance Etoile-Terre",
       y = "Fréquence")

summary(ech30)
skim(ech30)
Xbar30 = mean(ech30$Plx)
Xbar30
variance30 = var(ech30$Plx)
variance30
sigma30= sqrt(variance30)
n1=30

#1-b)
# Construction IC bilateral de Mu au seuil 1-alpha=95%
alpha1=0.05
qt((1-(alpha1/2)),n1-1) 
Ecart=qt((1-(alpha1/2)),n1-1)*sigma30/sqrt(n1)
Ecart
a=Xbar30-Ecart # La borne gauche
a
b=Xbar30+Ecart # La borne droite
b
print(paste("Xbar=",Xbar30,"IC",100*(1-alpha1),"%[",a,"-",b,"]"))

# Construction IC bilateral de Mu au seuil 1-alpha=99%
alpha2=0.01
qt((1-(alpha2/2)),n1-1) 
Ecart=qt((1-(alpha2/2)),n1-1)*sigma30/sqrt(n1)
Ecart
c=Xbar30-Ecart # La borne gauche
c
d=Xbar30+Ecart # La borne droite
d
print(paste("Xbar=",Xbar30,"IC",100*(1-alpha2),"%[",c,"-",d,"]"))


#1-c)
# Statistique de Test H0: Mu=Mu0
mutheo=7 #Mu0
t=sqrt(n1)*(Xbar30-mutheo)/sigma30
t
# quantile du test
qt((1-(alpha1/2)),n1-1)
# p-valeur du test
pval=2*(pt(abs(t),n1-1,lower.tail = FALSE))
pval
# Resultat du Test Mu=Mu0
print(paste(" T=",t,"ddl=",n1-1," p-valeur=",pval))

# Statistique de Test H0: Mu=Mu0
mutheo=7 #Mu0
t=sqrt(n1)*(Xbar30-mutheo)/sigma30
t
# quantile du test
qt((1-(alpha2/2)),n1-1)
# p-valeur du test
pval=2*(pt(abs(t),n1-1,lower.tail = FALSE))
pval
# Resultat du Test Mu=Mu0
print(paste(" T=",t,"ddl=",n1-1," p-valeur=",pval))


#1-d)
# Simulation dun echantillon de taille n de loi Normale desperance Mu
#Seuil a 95%
print(paste("Test de H0 : mu0 =", mutheo))
print(paste("Intervalle de confiance à", 100*(1 - alpha1), "% : [", a, "-", b, "]"))
#Seuil a 99%
print(paste("Test de H0 : mu0 =", mutheo))
print(paste("Intervalle de confiance à", 100*(1 - alpha2), "% : [", c, "-", d, "]"))


#1-e)
t.test(ech30$Plx,mu=7, conf.level=1-alpha1)

t.test(ech30$Plx,mu=7, conf.level=1-alpha2)

#1-f)
# Densite de la loi de T sous H0
curve(dt(x, n1-1), col="black", from=-3.5, to=3.5)
abline(v=t, lwd=2, col="blue")
abline(v=qt(1 - alpha1/2, n1-1), lwd=2, col="red")
abline(v=qt(alpha1/2, n1-1), lwd=2, col="red")

curve(dt(x, n1-1), col="black", from=-3.5, to=3.5)
abline(v=t, lwd=2, col="blue")
abline(v=qt(1 - alpha2/2, n1-1), lwd=2, col="red")
abline(v=qt(alpha2/2, n1-1), lwd=2, col="red")

#ECH100
#2-a)
#echantillon 100
ggplot(ech100, aes(y = Plx)) +
  geom_boxplot(fill = "lightblue", color = "darkblue") +
  labs(
    title = "Boxplot de la distance étoile-terre",
    y = "Distance Etoile-Terre")

ggplot(data = ech100) +
  aes(x = Plx) +
  geom_histogram(fill = "lightblue", color = "black") +
  labs(title = "Répartition de la distance étoile-terre",
       x = "Distance Etoile-Terre",
       y = "Fréquence")

summary(ech100)
skim(ech100)
Xbar100 = mean(ech100$Plx)
Xbar100
variance100 = var(ech100$Plx)
variance100
sigma100= sqrt(variance100)
n2=100

#2-b)
# Construction IC bilateral de Mu au seuil 1-alpha=95%
alpha1=0.05
qt((1-(alpha1/2)),n2-1) 
Ecart=qt((1-(alpha1/2)),n2-1)*sigma100/sqrt(n2)
Ecart
e=Xbar100-Ecart # La borne gauche
e
f=Xbar100+Ecart # La borne droite
f
print(paste("Xbar=",Xbar100,"IC",100*(1-alpha1),"%[",e,"-",f,"]"))

# Construction IC bilateral de Mu au seuil 1-alpha=99%
alpha2=0.01
qt((1-(alpha2/2)),n2-1) 
Ecart=qt((1-(alpha2/2)),n2-1)*sigma100/sqrt(n2)
Ecart
g=Xbar100-Ecart # La borne gauche
g
h=Xbar100+Ecart # La borne droite
h
print(paste("Xbar=",Xbar100,"IC",100*(1-alpha2),"%[",g,"-",h,"]"))


#2-c)
# Statistique de Test H0: Mu=Mu0
mutheo=7 #Mu0
t=sqrt(n2)*(Xbar100-mutheo)/sigma100
t
# quantile du test
qt((1-(alpha1/2)),n2-1)
# p-valeur du test
pval=2*(pt(abs(t),n2-1,lower.tail = FALSE))
pval
# Resultat du Test Mu=Mu0
print(paste(" T=",t,"ddl=",n2-1," p-valeur=",pval))

# Statistique de Test H0: Mu=Mu0
mutheo=7 #Mu0
t=sqrt(n2)*(Xbar100-mutheo)/sigma100
t
# quantile du test
qt((1-(alpha2/2)),n2-1)
# p-valeur du test
pval=2*(pt(abs(t),n2-1,lower.tail = FALSE))
pval
# Resultat du Test Mu=Mu0
print(paste(" T=",t,"ddl=",n2-1," p-valeur=",pval))


#2-d)
# Simulation dun echantillon de taille n de loi Normale desperance Mu
#Seuil a 95%
print(paste("Test de H0 : mu0 =", mutheo))
print(paste("Intervalle de confiance à", 100*(1 - alpha1), "% : [", e, "-", f, "]"))
#Seuil a 99%
print(paste("Test de H0 : mu0 =", mutheo))
print(paste("Intervalle de confiance à", 100*(1 - alpha2), "% : [", g, "-", h, "]"))


#2-e)
t.test(ech100$Plx,mu=7, conf.level=1-alpha1)

t.test(ech100$Plx,mu=7, conf.level=1-alpha2)

#2-f)
# Densite de la loi de T sous H0
curve(dt(x, n2-1), col="black", from=-3.5, to=3.5)
abline(v=t, lwd=2, col="blue")
abline(v=qt(1 - alpha1/2, n2-1), lwd=2, col="red")
abline(v=qt(alpha1/2, n2-1), lwd=2, col="red")

curve(dt(x, n2-1), col="black", from=-3.5, to=3.5)
abline(v=t, lwd=2, col="blue")
abline(v=qt(1 - alpha2/2, n2-1), lwd=2, col="red")
abline(v=qt(alpha2/2, n2-1), lwd=2, col="red")
