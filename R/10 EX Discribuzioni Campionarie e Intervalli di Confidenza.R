# ==== DISTRIBUZIONI CAMPIONARIE E INTERVALLI DI CONFIDENZA ===================

# Si consideri una popolazione normale di dimensione N=10000 
# con media 100 e varianza 25
# Si consideri un campione casuale di ampiezza n = 30

# a) Si definisca empiricamente la distribuzione campionaria della media 
# e si calcoli il suo valore atteso e la sua varianza, e si rappresenti
# la sua distribuzione

# b) Si definisca empiricamente  la distribuzione campionaria della varianza 
# e si calcoli il suo valore atteso e la sua varianza 
# e si rappresenti empiricamente

# c) Si definisca empiricamente la distribuzione campionaria della mediana 
# e si calcoli il suo valore atteso e la sua varianza
# e si rappresenti empiricamente

# d) Si calcoli un IDC per la media con livello di
# confidenza pari al 95% e si dimostri empiricamente
# l'adeguatezza del livello di confidenza

# e) Si calcoli un IDC per la varianza con livello di
# confidenza pari al 90% e si dimostri empiricamente
# l'adeguatezza del livello di confidenza
rm(list=ls())

# Parametri
N <- 10000
set.seed(123)
X <- rnorm(10000, 100, sqrt(25))
n <-30 # ampiezza campione
B <- 10000 # numero di campioni

# Vettori
medieC <- varianzeC <- medianeC <- varianzeCC <- rep(NA, B)
for(i in 1:B){
  s <- sample(X, n, replace = F)
  medieC[i] <- mean(s)
  medianeC[i]<-median(s)
  varianzeCC[i] <- var(s)
  varianzeC[i] <- varianzeCC[i] * ((n-1)/n)
}


# a) distribuzione campionaria della MEDIA ------------------------------------
medieC[1:10] # prime 10 medie

mean(medieC) # media delle medie

# La varianza delle medie deve essere uguale alla varianza 
# della popolazione divisa per n
var(medieC) # varianza delle medie
var(X)/n    # la varianza della popolazione divisa per n


plot(density(medieC), ylim=c(0,1))
lines(density(rnorm(10000, 100, 5/sqrt(n))), col=2)

# Metodo alternativo per confrontare la distribuzione delle medie con la normale
# Riusciamo a capire meglio cosa succede nelle code della distribuzione
qqplot(medieC,rnorm(10000, 100, 5/sqrt(n)), 
       main = "QQ Plot per la media campionaria")


# b) distribuzione campionaria della VARIANZA ---------------------------------
# Varianza Campionaria
plot(density((n * varianzeC) / var(X)), ylim=c(0,0.1))

# Per la varianza campionaria, la distribuzione e chi quadro 
# con n-1 gradi di liberta'
lines(density(rchisq(10000, n - 1)), col=2)
qqplot((n * varianzeC) / var(X),rchisq(10000, n - 1), 
       main="QQ Plot per la varianza campionaria")

# Varianza Campionaria Corretta
plot(density(((n - 1) * varianzeCC) / var(X)), ylim=c(0,.1))
lines(density(rchisq(10000, n - 1)), col=2)
qqplot(((n - 1) * varianzeCC) / var(X),rchisq(10000, n - 1), 
       main="QQ Plot per la varianza campionaria corretta")


# Verifichiamo che lo stimatore della varianza corretta e' corretto
# e che quindi ci restituisce un valore che e' uguale 
# alla varianza della popolazione
# Varianza campionaria
mean(varianzeC)
# Varianza campionaria corretta
mean(varianzeCC)


# c) distribuzione campionaria della MEDIANA ----------------------------------
# Non abbiamo studiato lo stimatore mediana campionaria
# ma possiamo fare un paragone con la media campionaria
mean(medianeC)
mean(medieC)
var(medianeC)

plot(density(medianeC), ylim=c(0,1))
lines(density(medieC), col=2)
qqplot(medianeC, medieC, 
       main="QQ Plot per la media e la mediana campionaria")


###############################################################################
# d) Intervallo di confidenza per la MEDIA ------------------------------------
# m = media campionaria
# alpha = livello di significativita'
# var = varianza della popolazione
# n = dimensione del campione

IDCm <- function(m, alpha, var, n){
  xx <- qnorm(alpha / 2) * sqrt(var) / sqrt(n)
  Linf <- m + xx
  Lsup <- m - xx
  return(c(Linf, Lsup))
}

# Testiamo la funzione sul primo valore di medieC
IDCm(medieC[1], 0.05, 25, n)

# Applichiamo la funzione a tutti i valori di medieC
IDCm_vec <- matrix(rep(NA), B, 2)
for(i in 1:B) 
  IDCm_vec[i,] <- IDCm(medieC[i], 0.05, 25, n)
IDCm_vec[1:5,]


# Dimostriamo che il 95% delle medie campionarie rientra 
# nell'intervallo di confidenza
# Devo verificarlo empiricamente, quindi devo andare a vedere quali
# intervalli contengono il valore 100
media_pop <- 100
cont <- 0
for(i in 1:B) 
  if(media_pop > IDCm_vec[i,1] && media_pop < IDCm_vec[i,2]) 
    cont <- cont + 1
cat("Il 95% delle medie campionarie rientra nell'intervallo di confidenza: ", 
    cont / 100,"%", "\n")

# IDCq <- quantile(medieC, c(.025, .975))
# IDCq

# Rappresentiamo graficamente gli intervalli di confidenza
par(bg="white")
# ? plot
plot(1:B, rep(100, B), type="n", ylim=c(90,110))
for(i in 1:B)
  lines(rep(i,2), IDCm_vec[i,], col=1, lwd=1)
lines(1:B, rep(100, B), lwd=2, col=2)


# Rappresentiamo graficamente gli intervalli di confidenza che 
# non contengono la media
par(bg="white")
plot(1:B, rep(100, B), type="n", ylim=c(90,110))
for(i in 1:B) {
  # Se la media della popolazione NON è all'interno dell'intervallo di 
  # confidenza, colora in fuchsia
  if(media_pop <= IDCm_vec[i,1] || media_pop >= IDCm_vec[i,2]) {
    lines(rep(i,2), IDCm_vec[i,], col="#ff00ff", lwd=1)
  } 
  #else {
  # lines(rep(i,2), IDCm_vec[i,], col=1, lwd=1)
  #}
}
lines(1:B, rep(100, B), lwd=2, col=2) # Linea della media della popolazione


# e) Intervallo di confidenza per la VARIANZA ---------------------------------
# s2 = varianza campionaria
# alpha = livello di significativita'
# n = dimensione del campione

IDCvar <- function(s2, alpha, n){
  chi1 <- qchisq(alpha/2, n - 1)
  chi2 <- qchisq(1 - alpha/2, n - 1)
  Linf <- (n * s2) / chi2
  Lsup <- (n * s2) / chi1
  return(c(Linf, Lsup))
}

IDCvar(varianzeC[1], 0.1, n)

IDCvar_vec <- matrix(rep(NA), B, 2)
for(i in 1:B) 
  IDCvar_vec[i,] <- IDCvar(varianzeC[i], 0.1, n)
IDCvar_vec[1:5,]

cont <- 0
for(i in 1:B) 
  if(25 > IDCvar_vec[i,1] && 25 < IDCvar_vec[i,2]) 
    cont <- cont + 1
cat("Il 90% delle varianze campionarie rientra nell'intervallo di confidenza: ", 
    cont / 100,"%", "\n")

par(bg="white")
# ? plot
plot(1:B, rep(100, B), type="n", ylim=c(0,100))
for(i in 1:B)
  lines(rep(i,2), IDCvar_vec[i,], col=1, lwd=1)
lines(1:B, rep(25, B), lwd=2, col=2)


# Rappresentiamo graficamente gli intervalli di confidenza 
# che non contengono la varianza
par(bg="white")
plot(1:B, rep(100, B), type="n", ylim=c(0,110))
for(i in 1:B) {
  # Se la varianza della popolazione NON è all'interno 
  # dell'intervallo di confidenza, 
  # colora in fuchsia
  if(25 < IDCvar_vec[i,1] || 25 > IDCvar_vec[i,2])  {
    lines(rep(i,2), IDCvar_vec[i,], col="#ff00ff", lwd=1)
  } 
  #else {
  # lines(rep(i,2), IDCm_vec[i,], col=1, lwd=1)
  #}
}
lines(1:B, rep(25, B), lwd=2, col=2) # Linea della media della popolazione


