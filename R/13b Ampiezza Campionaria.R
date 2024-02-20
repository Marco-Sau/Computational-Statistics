# ===== AMPIEZZA CAMPIONARIA ==================================================
rm(list = ls())
# Per la media -----------------------------------------------------------------
# Esercizio
sd <- 45
me <- 5 # Margine di errore
alpha <- 0.1

# Calcolo z
z <- qnorm(1 - alpha/2)
cat("Valore critico:", z, "\n")

# Calcolo la numerosità minima n
n <- (z * sd / me)^2
cat("La numerosità minima è:", ceiling(n), "\n")


# Funzione per il calcolo della numerosità minima
# Funzione per il calcolo della numerosità minima
num_min_media <- function(sd, me, alpha) {
  z <- qnorm(1 - alpha/2)
  n <- (z * sd / me)^2
  return(ceiling(n))
}

# Calcolo la numerosità minima n
n <- num_min_media(sd, me, alpha)
cat("La numerosità minima è:", n, "\n")


# Per la proporzione -----------------------------------------------------------
# Esercizio
me <- 0.03
alpha <- 0.05
stima <- 0.25

# Calcolo z
z <- qnorm(1 - alpha/2)
cat("Valore critico:", z, "\n")

# Calcolo la numerosità minima n
n <- (stima * (z^2)) / (me^2)
cat("La numerosità minima è:", ceiling(n), "\n")


# Funzione per il calcolo della numerosità minima
# Funzione per il calcolo della numerosità minima
num_min_proporzione <- function(stima, me, alpha) {
  z <- qnorm(1 - alpha/2)
  n <- (stima * (z^2)) / (me^2)
  return(ceiling(n))
}

# Calcolo la numerosità minima n
n <- num_min_proporzione(stima, me, alpha)
cat("La numerosità minima è:", n, "\n")