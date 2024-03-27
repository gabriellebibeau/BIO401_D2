metapopMech <- function(t, P, parms = c(c,e,N)){
  with(as.list(parms), { #Ouverture des vecteurs
    
    #Modèle (Nocc = P*N)
    dP <- c*P*N*(1-P) - e*P*N #dP/dt
    
    #Résultats
    res <- c(P=dP*P) #Proportion de sites colonisés
    return(list(res))
    
  }) #fin with
} #fin fonction