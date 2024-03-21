#modèle stochastique ----

metapopGill <- function(col, ext, N, Nini, tmax) {
  
  #Mise en jambes ;)
  pas <- 1 #pas de temps
  P0 <- Nini/N #Proportion initiale de sites occupés
  P_mat <- matrix(data = NA, nrow = tmax+1, ncol = 2, dimnames = list(seq(1,tmax+1),c('Pas_temps', 'P'))) #matrice pour regrouper tous les P
  P_mat[1,] <- c(0,P0) #Conditions initiales
  Nt <- Nini #Nombre de sites colonisés qui changera à chaque pas de temps
  
  
  #1. Définir les taux
  a <- c(col, ext) #2 taux spécifier au début de la fonction (colonisation et extinction)
  intervalle <- cumsum(a) #calcul des intervalles des évènements
  
  #2. Définir le taux global
  a0 <- sum(a) 
  
  while(pas <= tmax){ #Tant que le temps de simulation demandé n'est pas atteint, continuer!
    
  #3. Échantillonner temps t
  t <- runif(1,0,a0) #nombre aléatoire entre 0 et le taux global d'évènement
  
  #4. Échantillonner le type d'évènements
  if(t < intervalle[1]) {
    Nt <- Nt + 1 #Un site colonisé de plus
  }
  else {
    Nt <- Nt - 1 #Un site colonisé de moins
  }
  
  #5. Enregistrement de la donnée
  P <- Nt/N #Nouvelle proportion de sites colonisés
  P_mat[pas+1,] <- c(pas,P) #Attention la ligne 1 est le temps 0 dans la matrice
  
  pas <- pas+1 #avancement du pas de temps
  
  }#fin boucle while
  
  return(P_mat)
  
} #fin fonction
