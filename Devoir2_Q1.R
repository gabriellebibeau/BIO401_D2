#Mettre le bon working directory avec la fonction setwd()
#Librairies
library(ggplot2)
library(deSolve)

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


#Modèle ----
g_lin <- function(ac, R, C) { #taux de consommation de Bogustonia par Aleastonia
  res <- ac*R*C #linéaire
  return(res)
}

h_lin <- function(m, C) { #mortalité d'Aleastonia
  res <- m*C #taux constant
  return(res)
}

Bp_Ap_CR <- function(t, Cond_Ini, parms = c(r, ac, epsilon, m)){
  
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
r <- 0.15     
epsilon <- 1
m <- 0.4    
ac <- 0.02
para <- c(r=r, ac=ac, epsilon=epsilon, m=m)

soln <- ode(y=CI, times = seq(1,337), func = Bp_Ap_CR, parms = para)

ggplot() +
  geom_line(aes(soln[,'time'], soln[,'R']), color = 'cyan2') +
  geom_line(aes(soln[,'time'], soln[,'C']), color = 'salmon2') +
  geom_line(aes(donnees$X, donnees$bogustonia_proii), color = 'blue2') +
  geom_line(aes(donnees$X, donnees$aleastonia_predatora), color = 'red2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()


#Montecarlo - corrélation optimale ----

#Conditions initiales (selon l'énoncé)
R0 <- 25
C0 <- 20
CI <- c(R=R0, C=C0)

iterations <- 1
meilleure_correlation <- 0
meilleur_r <- 0
meilleur_ac <- 0
meilleur_m <- 0
epsilon <- 1

for(iterations in 1:10000){
  
  print(iterations)
  
  #Parametre
  r <- runif(1,0,1)
  ac <- runif(1,0,0.1)
  m <- runif(1,0,1)
  para_cor <- c(r=r, ac=ac, epsilon=epsilon, m=m)
  
  #calcul de la solution
  RC_soln <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para_cor)

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
        meilleur_r <- r
        meilleur_ac <- ac
        meilleur_m <- m
    } #fin if enregistrement
        
  } #fin if taille vecteurs
  
  iterations <- iterations + 1
  
}#fin boucle for

para_cor <- c(r=meilleur_r,ac=meilleur_ac,
              epsilon=1,m=meilleur_m)
soln_cor <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para_cor)

ggplot() +
  geom_line(aes(soln_cor[,'time'], soln_cor[,'R']), color = 'cyan2') +
  geom_line(aes(soln_cor[,'time'], soln_cor[,'C']), color = 'salmon2') +
  geom_line(aes(donnees$X, donnees$bogustonia_proii), color = 'blue2') +
  geom_line(aes(donnees$X, donnees$aleastonia_predatora), color = 'red2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()
