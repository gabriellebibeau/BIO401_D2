#Mettre le bon working directory avec la fonction setwd()
# 1. Librairies
library(ggplot2)
library(deSolve)


#Modèle de compétition Lotka-Voltera ----

Bp_Ap_LV <- function(t, Cond_Ini, parms = c(r_N, K_N, alpha_N, r_P, K_P, alpha_P)){ #N est dans les variables et non les paramètres
  
  with(as.list(Cond_Ini, parms), {
    
    #Croissance logistique
    dN = r_N*N*(1-((N + alpha_P*P)/K_N)) 
    dP = r_P*P*(1-((P + alpha_N*N)/K_P))
    
    # Resultat
    res <- c(dN=dN, dP=dP)
    return(list(res))
  })
}


#Conditions initiales (selon l'énoncé)
N0 <- 25
P0 <- 20
CI <- c(N=N0, P=P0)

#Paramètres
r_N <- 1.2
K_N <- 500
alpha_N <- 1 #influence de N sur P
r_P <- 1
K_P <- 300
alpha_P <- 2.785 #Influence de P sur N
parms <- c(r_N=r_N, K_N=K_N, alpha_N=alpha_N, 
           r_P=r_P, K_P=K_P,alpha_P=alpha_P)


#solution du système d'équations
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_LV, parms)

ggplot() +
  geom_line(aes(soln[,'time'], soln[,'N']), color = 'cyan2') +
  geom_line(aes(soln[,'time'], soln[,'P']), color = 'salmon2') +
  labs(title = "Modèle Lotka-Voltera", x = "Temps écoulé (1: 2h)", y = "Nombre d'individus") +
  coord_cartesian(xlim = c(0,150))+
  theme_minimal()


#Modèle Consommateur-Ressource ----

f_log <- function(t, R, R_max, r) { #Croissance de Bogustonia proii
  res <- r*R*(1-(R/R_max)) #logistique
  return(res)
}

f_exp <- function(t, R, r, alpha) { #Croissance de Bogustonia proii
  res <- r*R*exp(-alpha*R) #logistique
  return(res)
}

g_lin <- function(t, a, c, R, C) { #taux de consommation de Bogustonia par Aleastonia
  res <- a*c*R*C #linéaire
  return(res)
}

g_sat <- function(t,a,c,b,R){
  res <- a*c*R/(b+R)
  return(res)
}

h_lin <- function(t, m, C) { #mortalité d'Aleastonia
  res <- m*C #taux constant
  return(res)
}

Bp_Ap_CR1 <- function(t, Cond_Ini, parms = c(R_max, r, a, c, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Croissance logistique
    dR = f_log(t,R,R_max,r) - g_lin(t, a, c, R, C)
    dC = epsilon*g_lin(t, a, c, R, C) - h_lin(t, m, C)
    
    # Resultat
    res <- c(dR=dR, dC=dC)
    return(list(res))
  })
}

Bp_Ap_CR2 <- function(t, Cond_Ini, parms = c(r, alpha, a, c, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Croissance logistique
    dR = f_exp(t,R,r,alpha) - g_lin(t, a, c, R, C)
    dC = epsilon*g_lin(t, a, c, R, C) - h_lin(t, m, C)
    
    # Resultat
    res <- c(dR=dR, dC=dC)
    return(list(res))
  })
}

Bp_Ap_CR3 <- function(t, Cond_Ini, parms = c(R_max, r, a, c, b, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Croissance logistique
    dR = f_log(t,R,R_max,r) - g_sat(t,a,c,b,R)
    dC = epsilon*g_sat(t,a,c,b,R) - h_lin(t, m, C)
    
    # Resultat
    res <- c(dR=dR, dC=dC)
    return(list(res))
  })
}

#Conditions initiales (selon l'énoncé)
R0 <- 25
C0 <- 20
CI <- c(R=R0, C=C0)

#Paramètres
R_max <- 500 #500
r <- 3 #3
a <- 1
c <- 2.5 #2
epsilon <- 1
m <- 0.5 #1
alpha <- 1
b <- 1
parms1 <- c(R_max=R_max, r=r, a=a, c=c, epsilon=epsilon, m=m)
parms2 <- c(r=r,alpha=alpha, a=a, c=c, epsilon=epsilon, m=m)
parms3 <- c(R_max=R_max, r=r, a=a, c=c, b=b, epsilon=epsilon, m=m)

#solution du système d'équations
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR1, parms1)
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR2, parms2)
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR3, parms3)

ggplot() +
  geom_line(aes(soln[,'time'], soln[,'R']), color = 'cyan2') +
  geom_line(aes(soln[,'time'], soln[,'C']), color = 'salmon2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (1: 2h)", y = "Nombre d'individus") +
  coord_cartesian(xlim = c(0,337))+
  theme_minimal()


#Graphique données expérimentales ----

donnees <- read.csv('2Bacteries.csv')
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

