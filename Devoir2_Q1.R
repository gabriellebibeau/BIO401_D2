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


#Tests Modèle Consommateur-Ressource ----

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

Bp_Ap_CR1 <- function(t, Cond_Ini, parms = c(R_max, r, ac, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Modèle
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

Bp_Ap_CR4 <- function(t, Cond_Ini, parms = c(r, ac, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Modèle
    dR = R*r - g_lin(ac, R, C)
    dC = epsilon*g_lin(ac, R, C) - h_lin(m, C)
    
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
R_max <- 500  #500
r <- 3        #3
a <- 1        #1
c <- 2        #2
epsilon <- 1  #1
m <- 1      #1
alpha <- 0.1  
b <- 1
ac <- 2
parms1 <- c(R_max=R_max, r=r, a=a, c=c, epsilon=epsilon, m=m)
parms2 <- c(r=r,alpha=alpha, a=a, c=c, epsilon=epsilon, m=m)
parms3 <- c(R_max=R_max, r=r, a=a, c=c, b=b, epsilon=epsilon, m=m)
parms4 <- c(r=r, ac=ac, epsilon=epsilon, m=m)

#solution du système d'équations
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR1, parms1) #je pense que c'est le meilleur
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR2, parms2)
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR3, parms3)
soln <- ode(y=CI, times= seq(1,337), Bp_Ap_CR4, parms4)

ggplot() +
  geom_line(aes(soln[,'time'], soln[,'R']), color = 'cyan2') +
  geom_line(aes(soln[,'time'], soln[,'C']), color = 'salmon2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  coord_cartesian(xlim = c(0,337))+
  theme_minimal()

#Modèle finale juste in case ----

f_log <- function(R, R_max, r) { #Croissance de Bogustonia proii
  res <- r*R*(1-(R/R_max)) #logistique
  return(res)
}

g_lin <- function(ac, R, C) { #taux de consommation de Bogustonia par Aleastonia
  res <- ac*R*C #linéaire
  return(res)
}

h_lin <- function(m, C) { #mortalité d'Aleastonia
  res <- m*C #taux constant
  return(res)
}

Bp_Ap_CR <- function(t, Cond_Ini, parms = c(R_max, r, ac, epsilon, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Modèle
    dR = f_log(R,R_max,r) - g_lin(ac, R, C)
    dC = epsilon*g_lin(ac, R, C) - h_lin(m, C)
    
    # Resultat
    res <- c(dR=dR, dC=dC)
    return(list(res))
  })
}

CI_final <- c(R=25, C=20)
para_final <- c(R_max=260, r=2.8, ac=1.9, epsilon=1, m=0.5)
soln_final <- ode(y=CI_final, times= seq(1,337), func= Bp_Ap_CR, parms= para_final)

ggplot() +
  geom_line(aes(soln_final[,'time'], soln_final[,'R']), color = 'cyan2') +
  geom_line(aes(soln_final[,'time'], soln_final[,'C']), color = 'salmon2') +
  geom_line(aes(donnees$X, donnees$bogustonia_proii), color = 'blue2') +
  geom_line(aes(donnees$X, donnees$aleastonia_predatora), color = 'red2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
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
  labs(title = "Variation des population de A. predatora et B proii dans un pétri sur 2 semaines.", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()


#Montecarlo - corrélation optimale ----

#Conditions initiales (selon l'énoncé)
R0 <- 25
C0 <- 20
CI <- c(R=R0, C=C0)

iterations <- 1
meilleure_correlation <- 0
meilleur_Rmax <- 0
meilleur_r <- 0
meilleur_ac <- 0
meilleur_epsilon <- 0
meilleur_m <- 0

for(iterations in 1:1000){
  
  print(iterations)
  
  #Parametre
  R_max <- runif(1,35,500)
  r <- runif(1,2.5,3.5)
  ac <- runif(1,1,2)
  epsilon <- runif(1,0.5,1)
  m <- runif(1,0.5,1)
  para <- c(R_max=R_max, r=r, ac=ac, epsilon=epsilon, m=m)
  
  #calcul de la solution
  RC_soln <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para)

  if(length(donnees$bogustonia_proii) == nrow(RC_soln)){
  
  #Corrélation
    Bp <- as.numeric(RC_soln[,'R'])
    Pearson_Bp <- cor.test(Bp, donnees$bogustonia_proii, method = "pearson")
    estimate_Bp <- Pearson_Bp$estimate
    
    Ap <- as.numeric(RC_soln[,'C'])
    Pearson_Ap <- cor.test(Ap, donnees$aleastonia_predatora, method = "pearson")
    estimate_Ap <- Pearson_Ap$estimate
    
    estimate <- estimate_Bp + estimate_Ap
  
  #enregistrement des paramètres
    if(estimate > meilleure_correlation){
        meilleure_correlation <- estimate
        meilleur_Rmax <- R_max
        meilleur_r <- r
        meilleur_ac <- ac
        meilleur_epsilon <- epsilon
        meilleur_m <- m
    } #fin if enregistrement
        
  } #fin if taille vecteurs
  
  iterations <- iterations + 1
  
}#fin boucle for

para_cor <- c(R_max=meilleur_Rmax,r=meilleur_r,ac=meilleur_ac,
              epsilon=meilleur_epsilon,m=meilleur_m)
soln_cor <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para_cor)

ggplot() +
  geom_line(aes(soln_cor[,'time'], soln_cor[,'R']), color = 'cyan2') +
  geom_line(aes(soln_cor[,'time'], soln_cor[,'C']), color = 'salmon2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()
