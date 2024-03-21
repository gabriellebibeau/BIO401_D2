#Mettre le bon working directory avec la fonction setwd()
# 1. Librairies et données
library(ggplot2)
library(corrplot)
library(deSolve)

donnees <- read.csv('2Bacteries.csv')

#Modèle ressource-consommateur ----

Bp_Ap <- function(t, Cond_Ini, parms = c(r_N, K_N, alpha_N, r_P, K_P, alpha_P)){ #N est dans les variables et non les paramètres
  
  with(as.list(Cond_Ini, parms), {
    
    #Croissance logistique
    dN = r_N*N*(1-((N + alpha_P*P)/K_N))
    dP = r_P*P*(1-((P + alpha_N*N)/K_P))
    
    # Resultat
    res <- c(dN=dN, dP=dP)
    return(list(res))
  })
}


#Conditions initiales
N0 <- 25
P0 <- 20
CI <- c(N=N0, P=P0)

#Paramètres
r_N <- 0.4
K_N <- 40
alpha_N <- 1
r_P <- 0.5
K_P <- 30
alpha_P <- 1.5
parms <- c(r_N=r_N, K_N=K_N, alpha_N=alpha_N, 
           r_P=r_P, K_P=K_P,alpha_P=alpha_P)


#solution du système d'équations
soln <- ode(y=CI, times= seq(1,337), Bp_Ap, parms)
plot(soln[,'time'], soln[,'N'])
plot(soln[,'time'], soln[,'P'])

#� corriger
ggplot(aes()) +
  geom_line(soln[,'time'], soln[,'N'], color = 'cyan2') +
  geom_line(soln[,'time'], soln[,'P'], color = 'salmon2') +
  labs(title = "Mod�le", x = "Temps écoulé (1: 2h)", y = "Nombre d'individus") +
  theme_minimal()


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

