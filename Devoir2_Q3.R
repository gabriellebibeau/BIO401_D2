library(deSolve)
library(ggplot2)

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
mat_sto_100 <- matrix(NA, nrow = 61, ncol = 4)
mat_sto_100[,1] <- seq(0,60)
for (i in 1:3) {
  sto_100 <- metapopGill(c,e,N,N0,60)
  mat_sto_100[,i+1] <- sto_100[,'P']
}

ggplot()+
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,2]), color = 'red') +
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,3]), color = 'green') +
  geom_line(aes(mat_sto_100[,1], mat_sto_100[,4]), color = 'orange')


#Simulation méchanistique
mod_mec_100 <- ode(y=c(P=P0), times=seq(1,100), metapopMech, para) #monte à 0,5 très rapidement (t=2) et s'y stabilise
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
mat_sto_1000 <- matrix(NA, nrow = 61, ncol = 4)
mat_sto_1000[,1] <- seq(0,60)
for (i in 1:3) {
  sto_1000 <- metapopGill(c,e,N,N0,60)
  mat_sto_1000[,i+1] <- sto_1000[,'P']
}


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
mat_sto_10000 <- matrix(NA, nrow = 61, ncol = 4)
mat_sto_10000[,1] <- seq(0,60)
for (i in 1:3) {
  sto_10000 <- metapopGill(c,e,N,N0,60)
  mat_sto_10000[,i+1] <- sto_10000[,'P']
}


#Simulation méchanistique
mod_mec_10000 <- ode(y=P0, times=seq(1,3), metapopMech, para)
plot(mod_mec_10000[,'time'], mod_mec_10000[,'1'])


#Simulation N=100000 ----
#Conditions initiales
N0 <- 1
N <- 100000
P0 <- N0/N

#Paramètres
c <- 2
e <- 1
para <- c(c=c,e=e, N=N)

#Simulation stochastique
mat_sto_100000 <- matrix(NA, nrow = 61, ncol = 4)
mat_sto_100000[,1] <- seq(0,60)
for (i in 1:3) {
  sto_100000 <- metapopGill(c,e,N,N0,60)
  mat_sto_100000[,i+1] <- sto_100000[,'P']
}


#Simulation méchanistique
mod_mec_10000 <- ode(y=P0, times=seq(1,3), metapopMech, para)
plot(mod_mec_10000[,'time'], mod_mec_10000[,'1'])