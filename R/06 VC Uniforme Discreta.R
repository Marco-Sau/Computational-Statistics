# VARIABILE CASUALE UNIFORME DISCRETA =========================================

# Si scriva un codice in R in cui si definiscono :
# a) la funzione di (densità di) probabilità
# b) la  funzione di ripartizione
# c) il valore atteso e la varianza
# d) i principali quantili
# e) un insieme di 50 valori casuali
# f) il grafico della funzione di probabilità
# e) il grafico della funzione di ripartizione
# per la variabile casuale Uniforme discreta definita in (1,6)

###########
?dunif
# Funzione di probabilità generica ---------------------------------------------
Fprob_Unif <- function(x, min, max, plot=TRUE){
  max <- max + 1
  ris <- dunif(x, min, max)
  if(plot == TRUE){
  plot(min:(max-1), ris, type="h", xlab="Realizzazioni",
       ylab="F. di probabilità", 
       main = "V.C. Uniforme discreta")
  points(min:(max-1), ris, col=2, pch=16, cex =1.5)}
  return(ris)
}


# Funzione di ripartizione generica --------------------------------------------
Frip_Unif <- function(x, min, max, plot=TRUE){
  ris<- punif(x, min, max)
  if(plot == TRUE){
    plot(min:max, ris, type="s", xlab="Realizzazioni",
         ylab="F. di Ripartizione", 
         main = "V.C. Uniforme discreta")
    points(min:max, ris, col=2, pch=16, cex =1.5)}
  return(ris)
}


# Quantili principali ----------------------------------------------------------
Q_unif <- function(min, max){
  ris <- round(qunif(c(.01, .25, .5, .75, .99), min, max),0)
  return(ris)
}


# Distribuzione generica -------------------------------------------------------
R_unif <- function(n, min, max){
  ris <- round(runif(n, min, max),0)
  return(ris)
}


# Funzione principale ----------------------------------------------------------
VC_UniformeDiscreta <- function(x, n, min, max, plot=TRUE){
  fprob <- Fprob_Unif(x, min, max, plot=plot)
  frip <- Frip_Unif(x, min, max, plot=plot)
  nris <- (max - min) + 1
  va <- VA_unif <- (nris+1)/2
  var <- VAR_unif <- (nris^2-1)/12
  q <- Q_unif(min, max)
  r <- R_unif(n, min, max)
  
  # Stampa risultati
  cat("I valori della f. di prob. sono:", fprob, "\n")
  cat("I valori della f. di ripart. sono:", frip, "\n")
  cat("I principali quantili sono:", q, "\n")
  cat("Il valore atteso vale:", va, "\n")
  cat("La varianza vale", var, "\n")
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

xx <- VC_UniformeDiscreta(1:6, 50, 1, 6, plot = TRUE)
