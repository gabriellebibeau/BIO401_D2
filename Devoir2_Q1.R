#Mettre le bon working directory avec la fonction setwd()
# 1. Librairies et données
library(ggplot2)
library(corrplot)
library(deSolve)

donnees <- read.csv('2Bacteries.csv')

#Modèle ressource-consommateur ----

#Fonction B. proii
Bp <- function(t, N, parms_N = c(r_N, K_N)){ #N est dans les variables et non les paramètres
  
  with(as.list(parms_N), {
    
  #Croissance logistique
    dN = r_N*N*(1-N/K_N)
    
    # Resultat
    res <- dN
    return(list(res))
  })
}

#Fonction A. predatora
Ap <- function(t, P, parms_P = c(r_P, K_P)){ #N est dans les variables et non les paramètres
  
  with(as.list(parms_P), {
    
    #Croissance logistique
    dP = r_P*P*(1-P/K_P)
    
    # Resultat
    res <- dP
    return(list(res))
  })
}

#Faire juste un système d'équations avec les 2 

#Conditions initiales
N0 <- 25
P0 <- 20
CI <- c(N=N0, P=P0)

#Paramètres
r_N <- 0.4
K_N <- 40
parms_N <- c(r_N=r_N, K_N=K_N)

r_P <- 0.5
K_P <- 30
parms_P <- c(r_P=r_P, K_P=K_P)





#solution du système d'équations
bugostonia_model <- ode(y=c(N=N0), times= seq(1,337), bF, parms)
plot(bugostonia_model[,'time'], bugostonia_model[,'1'],)


#Graphique données expérimentales ----

bogustonia <- donnees[,3]
aleastonia <- donnees[,4]

x <- 1:length(bogustonia) #plus généraliste si on utilise la fonction length
y1 <- bogustonia
y2 <- aleastonia

df <- data.frame(x = c(x, x), y = c(y1, y2), group = rep(c("Bogustonia proii", "Aleastonia predatora"), each = length(x)))

ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line() +
  labs(title = "Variation des population de A. predatora et B proii dans un pétri sur 2 semaines.", x = "Temps écoulé (1: 2h)", y = "Nombre d'individus") +
  theme_minimal()

