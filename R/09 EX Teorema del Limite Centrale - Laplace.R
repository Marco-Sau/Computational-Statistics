# ======= TEOREMA DEL LIMITE CENTRALE ======================================
# ------- Teorema del limite centrale -----------------------------------------
media <- 220
sd <- 20
campione <- 100

# Calcolo della probabilità che il punteggio medio sia maggiore di 225
# 1 - pnorm(2.5)
ris <- 1 - pnorm(225, media, sd/sqrt(campione))
cat("La probabilità che il punteggio medio sia maggiore di 225 è:", ris, "\n")

# Calcolo della probabilità che il punteggio medio sia compreso tra 216 e 223
ris <- pnorm(223, media, sd/sqrt(campione)) - pnorm(216, media, sd/sqrt(campione))
cat("La probabilità che il punteggio medio sia compreso tra 216 e 223 è:", ris)



# ------- TEOREMA DI LAPLACE ------------------------------------------------
# Esercizio 1
# Dimostrare che il teorema di Laplace
# media di v.c. di Bernoulli indipendenti si distribuisce come una vc normale
# con media np e varianza np(1-p)
#      p = 0.05, 0.25, 0.5
# n = 10, 100, 1000, 10000


# Caso 1: p = 0.05, n = 10, 100, 1000, 10000 -------------------------------
set.seed(123)

media_teorica <- 0.05
varianza_teorica <- media_teorica * (1 - media_teorica)
cat("Media teorica: ", media_teorica, "\n")
cat("Varianza teorica: ", varianza_teorica, "\n")

xmedie10 <- xvarianze10 <- rep(0, 100)
xmedie100 <- xvarianze100 <- rep(0, 100)
xmedie1000 <- xvarianze1000 <- rep(0, 100)
xmedie10000 <- xvarianze10000 <- rep(0, 100)

for (i in 1:100){
  x10 <- rbinom(10, 1, media_teorica)
  m10 <- mean(x10)
  v10 <- m10 * (1 - m10)
  xmedie10[i] <- m10
  xvarianze10[i] <- v10
}
cat("Media di 10 v.c. di Bernoulli indipendenti: ", mean(xmedie10), "\n")
cat("Varianza di 10 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze10), "\n")

for (i in 1:100){
  x100 <- rbinom(100, 1, media_teorica)
  m100 <- mean(x100)
  v100 <- m100 * (1 - m100)
  xmedie100[i] <- m100
  xvarianze100[i] <- v100
}
cat("Media di 100 v.c. di Bernoulli indipendenti: ", mean(xmedie100), "\n")
cat("Varianza di 100 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze100), "\n")

for (i in 1:100){
  x1000 <- rbinom(1000, 1, media_teorica)
  m1000 <- mean(x1000)
  v1000 <- m1000 * (1 - m1000)
  xmedie1000[i] <- m1000
  xvarianze1000[i] <- v1000
}
cat("Media di 1000 v.c. di Bernoulli indipendenti: ", mean(xmedie1000), "\n")
cat("Varianza di 1000 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze1000), "\n")

for (i in 1:100){
  x10000 <- rbinom(10000, 1, media_teorica)
  m10000 <- mean(x10000)
  v10000 <- m10000 * (1 - m10000)
  xmedie10000[i] <- m10000
  xvarianze10000[i] <- v10000
}


# Set up the plotting area
par(bg = "white")
par(mfrow = c(2, 2))


hist(xmedie10, main = "Media di 10 v.c. di Bernoulli indipendenti", 
     xlab = "Media",
     xlim = c(0, 0.2), 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie100, main = "Media di 100 v.c. di Bernoulli indipendenti",
     xlab = "Media", 
     xlim = c(0, 0.12), 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/100)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie1000, main = "Media di 1000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     xlim = c(0.03, 0.07), 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/1000)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie10000, main = "Media di 10000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     xlim = c(0.043, 0.057), 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10000)), 
      col = "red", lwd = 2, add = TRUE)


# Caso 2: p = 0.25, n = 10, 100, 1000, 10000 -------------------------------
set.seed(123)

media_teorica <- 0.25
varianza_teorica <- media_teorica * (1 - media_teorica)
cat("Media teorica: ", media_teorica, "\n")
cat("Varianza teorica: ", varianza_teorica, "\n")

xmedie10 <- xvarianze10 <- rep(0, 100)
xmedie100 <- xvarianze100 <- rep(0, 100)
xmedie1000 <- xvarianze1000 <- rep(0, 100)
xmedie10000 <- xvarianze10000 <- rep(0, 100)

for (i in 1:100){
  x10 <- rbinom(10, 1, media_teorica)
  m10 <- mean(x10)
  v10 <- m10 * (1 - m10)
  xmedie10[i] <- m10
  xvarianze10[i] <- v10
}
cat("Media di 10 v.c. di Bernoulli indipendenti: ", 
    mean(xmedie10), "\n")
cat("Varianza di 10 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze10), "\n")

for (i in 1:100){
  x100 <- rbinom(100, 1, media_teorica)
  m100 <- mean(x100)
  v100 <- m100 * (1 - m100)
  xmedie100[i] <- m100
  xvarianze100[i] <- v100
}
cat("Media di 100 v.c. di Bernoulli indipendenti: ", 
    mean(xmedie100), "\n")
cat("Varianza di 100 v.c. di Bernoulli indipendenti: ",
    mean(xvarianze100), "\n")

for (i in 1:100){
  x1000 <- rbinom(1000, 1, media_teorica)
  m1000 <- mean(x1000)
  v1000 <- m1000 * (1 - m1000)
  xmedie1000[i] <- m1000
  xvarianze1000[i] <- v1000
}
cat("Media di 1000 v.c. di Bernoulli indipendenti: ", 
    mean(xmedie1000), "\n")
cat("Varianza di 1000 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze1000), "\n")

for (i in 1:100){
  x10000 <- rbinom(10000, 1, media_teorica)
  m10000 <- mean(x10000)
  v10000 <- m10000 * (1 - m10000)
  xmedie10000[i] <- m10000
  xvarianze10000[i] <- v10000
}


# Set up the plotting area
par(bg = "white")
par(mfrow = c(2, 2))


hist(xmedie10, main = "Media di 10 v.c. di Bernoulli indipendenti", 
     xlab = "Media",
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie100, main = "Media di 100 v.c. di Bernoulli indipendenti",
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/100)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie1000, main = "Media di 1000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/1000)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie10000, main = "Media di 10000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10000)), 
      col = "red", lwd = 2, add = TRUE)


# Caso 3: p = 0.5, n = 10, 100, 1000, 10000 -------------------------------
set.seed(123)

media_teorica <- 0.5
varianza_teorica <- media_teorica * (1 - media_teorica)
cat("Media teorica: ", media_teorica, "\n")
cat("Varianza teorica: ", varianza_teorica, "\n")

xmedie10 <- xvarianze10 <- rep(0, 100)
xmedie100 <- xvarianze100 <- rep(0, 100)
xmedie1000 <- xvarianze1000 <- rep(0, 100)
xmedie10000 <- xvarianze10000 <- rep(0, 100)

for (i in 1:100){
  x10 <- rbinom(10, 1, media_teorica)
  m10 <- mean(x10)
  v10 <- m10 * (1 - m10)
  xmedie10[i] <- m10
  xvarianze10[i] <- v10
}
cat("Media di 10 v.c. di Bernoulli indipendenti: ", mean(xmedie10), "\n")
cat("Varianza di 10 v.c. di Bernoulli indipendenti: ", mean(xvarianze10), "\n")

for (i in 1:100){
  x100 <- rbinom(100, 1, media_teorica)
  m100 <- mean(x100)
  v100 <- m100 * (1 - m100)
  xmedie100[i] <- m100
  xvarianze100[i] <- v100
}
cat("Media di 100 v.c. di Bernoulli indipendenti: ", mean(xmedie100), "\n")
cat("Varianza di 100 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze100),"\n")

for (i in 1:100){
  x1000 <- rbinom(1000, 1, media_teorica)
  m1000 <- mean(x1000)
  v1000 <- m1000 * (1 - m1000)
  xmedie1000[i] <- m1000
  xvarianze1000[i] <- v1000
}
cat("Media di 1000 v.c. di Bernoulli indipendenti: ", mean(xmedie1000), "\n")
cat("Varianza di 1000 v.c. di Bernoulli indipendenti: ", 
    mean(xvarianze1000), "\n")

for (i in 1:100){
  x10000 <- rbinom(10000, 1, media_teorica)
  m10000 <- mean(x10000)
  v10000 <- m10000 * (1 - m10000)
  xmedie10000[i] <- m10000
  xvarianze10000[i] <- v10000
}


# Set up the plotting area
par(bg = "white")
par(mfrow = c(2, 2))


hist(xmedie10, main = "Media di 10 v.c. di Bernoulli indipendenti", 
     xlab = "Media",
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie100, main = "Media di 100 v.c. di Bernoulli indipendenti",
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/100)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie1000, main = "Media di 1000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/1000)), 
      col = "red", lwd = 2, add = TRUE)

hist(xmedie10000, main = "Media di 10000 v.c. di Bernoulli indipendenti", 
     xlab = "Media", 
     freq = FALSE)
curve(dnorm(x, mean = media_teorica, sd = sqrt(varianza_teorica/10000)), 
      col = "red", lwd = 2, add = TRUE)


# Esercizio 2 ---------------------------------------------------------------
n = 100
p = 1/8

# Calcolare le probabilità che X sia ≥ di 6 e ≤ di 10
ris <- pbinom(10, n, p) - pbinom(6, n, p)
cat("La probabilità che (6 <= X <= 10) è:", ris, "\n")

# Calcolare la probabilità utilizzando la funzione pnorm
ris <- pnorm(10, n*p, sqrt(n*p*(1-p))) - pnorm(6, n*p, sqrt(n*p*(1-p)))
cat("La probabilità che (6 <= X <= 10) è:", ris, "\n")