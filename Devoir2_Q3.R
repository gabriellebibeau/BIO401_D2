library(deSolve)

#modèle stochastique ----
source('Devoir2_Q2.R')


#modèle méchanistique ----
metapopMech <- function(t, P, parms = c(c,e,N)){
  with(as.list(parms), {
    
    #Modèle (Nocc = P*N)
    dP <- c*P*N*(1-P) - e*P*N #dP/dt
    
    #Résultats
    res <- P*dP
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
mod_sto_100 <- metapopGill(c,e,N,N0,3) #Extinction, que faire?
plot(mod_sto_100)

#Simulation méchanistique
mod_mec_100 <- ode(y=c(P=P0), times=seq(1,3), metapopMech, para) #monte à 0,5 très rapidement (t=2) et s'y stabilise
plot(mod_mec_100[,'time'], mod_mec_100[,'P'])
#pas probl?matique, voir cours 11 diapo 23


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
mod_sto_1000 <- metapopGill(c,e,N,N0,3) 
plot(mod_sto_1000)

#Simulation méchanistique
mod_mec_1000 <- ode(y=P0, times=seq(1,3), metapopMech, para)
plot(mod_mec_1000[,'time'], mod_mec_1000[,'1'])


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
mod_sto_10000 <- metapopGill(c,e,N,N0,3) 
plot(mod_sto_10000)

#Simulation méchanistique
mod_mec_10000 <- ode(y=P0, times=seq(1,3), metapopMech, para)
plot(mod_mec_10000[,'time'], mod_mec_10000[,'1'])