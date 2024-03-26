#Chargement de librairie
library(deSolve)
library(ggplot2)
library(patchwork)

#modèle stochastique ----
source('Devoir2_Q2.R')


#modèle méchanistique ----
metapopMech <- function(t, P, parms = c(c,e,N)){
  with(as.list(parms), { #Ouverture des vecteurs
    
    #Modèle (Nocc = P*N)
    dP <- c*P*N*(1-P) - e*P*N #dP/dt
    
    #Résultats
    res <- c(P=dP*P) #Proportion de sites colonisés
    return(list(res))
    
  }) #fin with
} #fin fonction


#Simulation N=100 ----
#Conditions initiales
N0 <- 1
N <- 100
P0 <- N0/N

#Paramètres
c <- 2
e <- 1
para <- c(c=c,e=e, N=N)

#Simulation stochastique
mat_sto_100 <- matrix(NA, nrow = 51, ncol = 4) #Création d'une matrice pour enregistrée les données
mat_sto_100[,1] <- seq(0,50) #La première colonne est les pas de temps
for (i in 1:3) { #Boucle pour produire 3 résultats différents
  sto_100 <- metapopGill(c,e,N,N0,50) #Résultat du modèle stochastique
  mat_sto_100[,i+1] <- sto_100[,'P'] #Enregistrement des solutions
}


a <- ggplot()+
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,2]), color = 'salmon') +
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,3]), color = 'red') +
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,4]), color = 'pink3')+
  labs(title = "N=100 : Modèle Stochastique", x = "Pas de temps", y = "Proportion de sites colonisés")


#Simulation méchanistique
mec_100 <- ode(y=c(P=P0), times=seq(1,50), metapopMech, para) #monte à 0,5 très rapidement (t=2) et s'y stabilise

b <- ggplot()+
  geom_line(aes(mec_100[,'time'], mec_100[,'P']), color = 'green3') +
  labs(title = "Modèle Méchanistique", x = "Pas de temps", y = "Proportion de sites colonisés")
#pas probl?matique, voir cours 11 diapo 23
a|b


#Simulation N=1000 ----
#Conditions initiales
N0 <- 1
N <- 1000
P0 <- N0/N

#Paramètres
c <- 2
e <- 1
para <- c(c=c,e=e, N=N)

#Simulation stochastique
mat_sto_1000 <- matrix(NA, nrow = 51, ncol = 4) #Création d'une matrice pour enregistrée les données
mat_sto_1000[,1] <- seq(0,50) #La première colonne est les pas de temps
for (i in 1:3) { #Boucle pour produire 3 résultats différents
  sto_1000 <- metapopGill(c,e,N,N0,50) #Résultat du modèle stochastique
  mat_sto_1000[,i+1] <- sto_1000[,'P'] #Enregistrement des solutions
}

c <- ggplot()+
  geom_line(aes(mat_sto_1000[,1], mat_sto_1000[,2]), color = 'salmon') +
  geom_line(aes(mat_sto_1000[,1], mat_sto_1000[,3]), color = 'red') +
  geom_line(aes(mat_sto_1000[,1], mat_sto_1000[,4]), color = 'pink3')+
  labs(title = "N=1000 : Modèle Stochastique", x = "Pas de temps", y = "Proportion de sites colonisés")

#Simulation méchanistique
mec_1000 <- ode(y=P0, times=seq(1,50), metapopMech, para)
d <- ggplot()+
  geom_line(aes(mec_1000[,'time'], mec_1000[,'1']), color = 'green3') +
  labs(title = "Modèle Méchanistique", x = "Pas de temps", y = "Proportion de sites colonisés")

c|d

#Simulation N=10000 ----
#Conditions initiales
N0 <- 1
N <- 10000
P0 <- N0/N

#Paramètres
c <- 2
e <- 1
para <- c(c=c,e=e, N=N)

#Simulation stochastique
mat_sto_10000 <- matrix(NA, nrow = 51, ncol = 4) #Création d'une matrice pour enregistrée les données
mat_sto_10000[,1] <- seq(0,50) #La première colonne est les pas de temps
for (i in 1:3) { #Boucle pour produire 3 résultats différents
  sto_10000 <- metapopGill(c,e,N,N0,50) #Résultat du modèle stochastique
  mat_sto_10000[,i+1] <- sto_10000[,'P'] #Enregistrement des solutions
}

e <- ggplot()+
  geom_line(aes(mat_sto_10000[,1], mat_sto_10000[,2]), color = 'salmon') +
  geom_line(aes(mat_sto_10000[,1], mat_sto_10000[,3]), color = 'red') +
  geom_line(aes(mat_sto_10000[,1], mat_sto_10000[,4]), color = 'pink3')+
  labs(title = "N=10 000 : Modèle Stochastique", x = "Pas de temps", y = "Proportion de sites colonisés")


#Simulation méchanistique
mec_10000 <- ode(y=P0, times=seq(1,50), metapopMech, para)

f <- ggplot()+
  geom_line(aes(mec_10000[,'time'], mec_10000[,'1']), color = 'green3') +
  labs(title = "Modèle Méchanistique", x = "Pas de temps", y = "Proportion de sites colonisés")

e|f
