#Devoir 2

library(ggplot2)
library(corrplot)
library(deSolve)

#À ÉDITER, EN FONCTION DE TON DOCUMENT. Le simple titre du document ne fonctionnait pas
## 1. Initialisation des données
donnees <- read.csv('/Users/zachariescalzo/Desktop/Maitrise/Cours/Modélisation des systèmes biologiques/Devoir 2/2Bacterie.csv')

#Courbe Bugostonia

bF <- function(t, vars, parms = c(r, N, K)){
  
  with(as.list(vars, parms), {
    
  #Croissance logistique
    dN = r*N*(1-N/K)
    
    # Resultat
    res <- c(dN)
    return(list(res))
  })
}


#Conditions initiales
N0 = 25
CI = N=N0
#Paramètres
r = 0.4
K = 30


parms <- c(r=r, K=K)

bugostonia_model <- ode(CI, seq(1,337), bF, parms)
plot(bugostonia_model[,'time'], bugostonia_model[,'1'],)

#Courbe Bogustonia


bogustonia <- c(donnees[,3])
aleastonia <- c(donnees[,4])

x <- 1:337
y1 <- bogustonia
y2 <- aleastonia

df <- data.frame(x = c(x, x), y = c(y1, y2), group = rep(c("Bogustonia proii", "Aleastonia predatora"), each = length(x)))

ggplot(df, aes(x = x, y = y, color = group)) +
  geom_line() +
  labs(title = "Données", x = "X-axis", y = "Y-axis") +
  theme_minimal()

