# ===== POTENZA DEL TEST ======================================================
rm(list = ls())
# test unidirezionale a sinistra per la media ----------------------------------
# Test unidirezionale a sinistra
# H0 : mu = 52
# H1 : mu < 52

m0 <- 52 
n <- 64 

# vettore dei possibili parametri sotto H1
h1_true <- seq(m0 - 0.025, m0 - (50 * .25), by = - 0.025)
sigma <- 6
alpha <- .05

z_alpha <- qnorm(alpha) # mi serve per definire il valore critico
x_crit <- m0 - z_alpha * sigma / sqrt(n)
cat("Valore critico:", x_crit, "\n")

beta <- rep(0, length(h1_true))
for (i in 1 : length(h1_true)) {
  beta[i] <- 1 - pnorm((x_crit - h1_true[i]) / (sigma / sqrt(n)))
}
potenza <- 1 - beta

par(bg = "white")
plot(h1_true, potenza, 
     type = "l", 
     xlab = "h1", 
     ylab = "potenza", 
     main = "funzione potenza")


# test uni-direzionale a destra per la media -----------------------------------
# H0: mu = 52
# H1: mu > 52

m0 <- 52
n <- 64 
h1_true <- seq(m0 + 0.25, m0 + (50 * .25), by = 0.025)
varianza <- 6^2
alpha <- .05

z_alpha <- qnorm(1 - alpha) 
x_crit <- m0 + (z_alpha * sqrt(varianza) / sqrt(n))
cat("Valore critico:", x_crit, "\n")

beta <- rep(0, length(h1_true))
for (i in 1 : length(h1_true)) {
  beta[i] <- 1 - pnorm((x_crit - h1_true[i]) / (sigma / sqrt(n)))
}
potenza <- 1 - beta

par(bg = "white")
plot(h1_true, potenza, 
     type = "l", 
     xlab = "h1", 
     ylab = "potenza", 
     main = "funzione potenza")

# Per tutti i valori della media campionaria che sono minori del valore critico,
# non rifiuto H0.
# Per tutti i valori della media campionaria che sono maggiori del valore critico,
# rifiuto H0.


# test bi-direzionale per la media ---------------------------------------------
# test bi-direzionale
# H0: mu = 52
# H1: mu != 52

m0 <- 52
n <- 64
h1_true <- seq(m0 - (50 * .25), m0 + (50 * .25), by = 0.025)

varianza <- 6^2
alpha <- .05
alpha2 <- alpha / 2

z_alpha <- abs(qnorm(alpha))
z_alpha2 <- abs(qnorm(alpha2))

x_crit1 <- m0 - (z_alpha2 * sigma / sqrt(n))
x_crit2 <- m0 + (z_alpha2 * sigma / sqrt(n))
x_crit <- m0 + (z_alpha * sigma / sqrt(n))
cat("Valori critici:", x_crit1, x_crit2, "\n")

beta <- rep(0, length(h1_true))
for (i in 1 : length(h1_true)) {
  if(h1_true[i] <= m0) {
    beta[i] <- 1 - pnorm((x_crit1 - h1_true[i]) / (sigma / sqrt(n)))
  } else {
    beta[i] <- pnorm((x_crit2 - h1_true[i]) / (sigma / sqrt(n)))
  }
}
potenza <- 1 - beta

par(bg = "white")
plot(h1_true, potenza, 
     type = "l", 
     xlab = "h1", 
     ylab = "potenza", 
     main = "funzione potenza")


# Funzione per il calcolo della Potenza del Test ------------------------------
# Funzione per il calcolo della potenza del test con distribuzione normale e scelta del tipo di test, 
# bilaterale, uni-laterale a sinistra o uni-laterale a destra
calcola_potenza_test_normale <- function(media_nulla, media_reale, sd, dimensione_campione, alpha, tipo_test) {
  # Calcolo del valore critico Z
  if (tipo_test == "bilaterale") {
    z_critico <- qnorm(1 - (alpha / 2))
  } else if (tipo_test == "unilaterale_sinistra") {
    z_critico <- qnorm(1 - alpha)
  } else if (tipo_test == "unilaterale_destra") {
    z_critico <- qnorm(alpha)
  } else {
    print("Tipo di test non valido")
    return(0)
  }
  
  # Calcolo del valore Z per l'ipotesi alternativa
  z <- (media_reale - media_nulla) / (sd / sqrt(dimensione_campione))
  
  # Calcolo della potenza del test
  if (tipo_test == "bilaterale") {
    potenza <- 1 - pnorm(z_critico - z) + pnorm(-z_critico - z)
  } else if (tipo_test == "unilaterale_sinistra") {
    potenza <- 1 - pnorm(z_critico - z)
  } else if (tipo_test == "unilaterale_destra") {
    potenza <- pnorm(z_critico - z)
  }
  
  return(potenza)
}

# Esempio di utilizzo della funzione
# Parametri:
# media_nulla = 50, (mu*)
# media_reale = 52, (mu)
# deviazione_standard = 6, 
# dimensione_campione = 30, 
# significativita = 0.05, 
# tipo_test = "bilaterale", "unilaterale_sinistra", "unilaterale_destra"
esempio_potenza <- calcola_potenza_test_normale(50, 52, 6, 64, 0.05, "bilaterale")
cat("Potenza del test (Bilaterale):", esempio_potenza, "\n")

esempio_potenza <- calcola_potenza_test_normale(50, 52, 6, 64, 0.05, "unilaterale_sinistra")
cat("Potenza del test (Unilaterale a sinistra):", esempio_potenza, "\n")

esempio_potenza <- calcola_potenza_test_normale(50, 52, 6, 64, 0.05, "unilaterale_destra")
cat("Potenza del test (Unilaterale a destra):", esempio_potenza, "\n")
