# ==== Intervalli di accettazione =============================================

# Esempio 1: Intervallo di accettazione già noto ------------------------------
# impotizzando che mu = 100
# di estrarre un campione di 35 osservazioni
# che la varianza della popolazione sia uguale a 25

# Calcolare la prob. Che la media del campione sia compresa tra 80 e 120

# P( x_camp sin in 870 -- 120) = 
# P(100 - (80-100)/sigma/rad(n)< x_camp < 100 + (120-100)/sigma/rad(n))

z1 <- (80 - 100) * (sqrt(25) / sqrt(35))
z1
z2 <- (120 - 100) * (sqrt(25) / sqrt(35))
z2

pnorm(z2) - pnorm(z1)
cat("The probability is", pnorm(z2) - pnorm(z1), "\n")


# Esempio 2: intervallo di accettazione da determinare ------------------------
# impotizzando che mu = 100
# di estrarre un campione di 35 osservazioni
# che la varianza della popolazione sia uguale a 25

# Calcolare l'intervallo entro il quale con probabilità 0.94 sia compresa la 
# media campionaria

z1 <- qnorm(0.03)
z2 <- qnorm(0.97)

l_inf <- 100 + z1 * (sqrt(25) / sqrt(35))
# l_inf
l_sup <- 100 + z2 * (sqrt(25) / sqrt(35))
# l_sup
cat("L'intervallo di confidenza è [", l_inf, ";", l_sup, "]\n")

