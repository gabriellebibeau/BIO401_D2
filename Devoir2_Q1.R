##      Mettre le bon working directory avec la fonction setwd()    ##
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


#Modèle Final----

f_lin <- function(r, R) { #croissance de Bogustonia
  res <- r*R #taux constant
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

Bp_Ap_CR <- function(t, Cond_Ini, parms = c(r, ac, m)){
  
  with(as.list(Cond_Ini, parms), {
    
    #Modèle
    dR = f_lin(r, R) - g_lin(ac, R, C)
    dC = g_lin(ac, R, C) - h_lin(m, C) #epsilon devrait apparaitre devait g_lin(), mais nous assumons qu'il est égal à 1 pour simplifier
    
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
r <- 0.241
ac <- 0.045
m <- 0.408
para <- c(r=r, ac=ac,m=m)

soln <- ode(y=CI, times = seq(1,337), func = Bp_Ap_CR, parms = para)

ggplot() +
  geom_line(aes(soln[,'time'], soln[,'R']), color = 'cyan2') +
  geom_line(aes(soln[,'time'], soln[,'C']), color = 'salmon2') +
  geom_line(aes(donnees$X, donnees$bogustonia_proii), color = 'blue2') +
  geom_line(aes(donnees$X, donnees$aleastonia_predatora), color = 'red2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()

#Test de corrélation final
Bp <- as.numeric(soln[,'R']) #Extraction des tailles de population de B. proii
Pearson_Bp <- cor.test(Bp, donnees$bogustonia_proii, method = "pearson") #test de Pearson
Pearson_Bp$estimate

Ap <- as.numeric(soln[,'C']) #Extraction des tailles de population de A. predatoria
Pearson_Ap <- cor.test(Ap, donnees$aleastonia_predatora, method = "pearson") #test de Pearson
Pearson_Ap$estimate


#Montecarlo - corrélation optimale ----

#Conditions initiales (selon l'énoncé)
R0 <- 25
C0 <- 20
CI <- c(R=R0, C=C0)

#Préparation des paramètres finaux
meilleure_correlation <- 0
meilleur_r <- 0
meilleur_ac <- 0
meilleur_m <- 0

for(iterations in 1:5){ #boucle pour tester plusieurs variables
  
  #Paramètre (estimer d'abord à tâton)
  r <- runif(1,0,0.5)
  ac <- runif(1,0,0.5)
  m <- runif(1,0,0.5)
  para_cor <- c(r=r, ac=ac, m=m)
  
  #calcul de la solution
  RC_soln <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para_cor)

  if(length(donnees$bogustonia_proii) == nrow(RC_soln)){ #Si la solution ode n'a pas plantée
  
  #Corrélation
    Ap <- as.numeric(RC_soln[,'C']) #Extraction des tailles de population de A. predatoria
    Pearson_Ap <- cor.test(Ap, donnees$aleastonia_predatora, method = "pearson") #test de Pearson
    estimate_Ap <- Pearson_Ap$estimate
    
    estimate <- estimate_Ap #+ estimate_Bp
  
  #enregistrement des paramètres
    if(estimate > meilleure_correlation){
        meilleure_correlation <- estimate
        meilleur_r <- r
        meilleur_ac <- ac
        meilleur_m <- m
    } #fin if enregistrement
        
  } #fin if taille vecteurs
  
}#fin boucle for

para_cor <- c(r=meilleur_r,ac=meilleur_ac,m=meilleur_m) #enregistrement des paramètres finaux
soln_cor <- ode(y=CI, times= seq(1,337), func= Bp_Ap_CR, parms= para_cor) #solution finale

ggplot() + #illustration du modèle et des données
  geom_line(aes(soln_cor[,'time'], soln_cor[,'R']), color = 'cyan2') +
  geom_line(aes(soln_cor[,'time'], soln_cor[,'C']), color = 'salmon2') +
  geom_line(aes(donnees$X, donnees$bogustonia_proii), color = 'blue2') +
  geom_line(aes(donnees$X, donnees$aleastonia_predatora), color = 'red2') +
  labs(title = "Modèle Consommateur-Ressource", x = "Temps écoulé (h)", y = "Nombre d'individus") +
  theme_minimal()


###############################
Ap <- as.numeric(RC_soln[,'C']) #Extraction des tailles de population de A. predatoria
Pearson_Ap <- cor.test(Ap, donnees$aleastonia_predatora, method = "pearson") #test de Pearson
estimate_Ap <- Pearson_Ap$estimate

Bp <- as.numeric(RC_soln[,'R']) #Extraction des tailles de population de B. proii
Pearson_Bp <- cor.test(Bp, donnees$bogustonia_proii, method = "pearson") #test de Pearson
estimate_Bp <- Pearson_Bp$estimate
