# VARIABILE CASUALE NORMALE CONTINUA =========================================

# Si scriva un codice in R in cui si definiscono :
# a) la funzione di (densità di) probabilità
# b) la  funzione di ripartizione
# c) il valore atteso e la varianza
# d) i principali quantili
# e) un insieme di 50 valori casuali
# f) il grafico della funzione di probabilità
# e) il grafico della funzione di ripartizione
# per la variabile casuale Normale con media 5 e varianza 4

#dnorm( 2, 5, sqrt(4))
#? dnorm

media<-5
sd<-sqrt(4)

# Funzione di densità di probabilità
Fprob_Norm <- function(x, media, sd, plot=TRUE){
  if(is.null(x)) x <- seq(qnorm(.01, media, sd),
                          qnorm(.99, media, sd),
                          length.out = 100)
  ris<- dnorm(x, media, sd)
  if(plot == TRUE){
  plot(x, ris, type="h", xlab="Realizzazioni",
       ylab="F. di densità di probabilità", 
       main = "V.C. Normale")
  points(x, ris, col=2, pch=16, cex =1.5)}
  return(ris)
}

# Funzione di ripartizione
Frip_Norm <- function(x, media, sd, plot=TRUE){
  if(is.null(x)) x <- seq(qnorm(.01, media, sd),
                          qnorm(.99, media, sd),
                          length.out = 100)
  ris<- pnorm(x, media, sd)
  if(plot == TRUE){
    plot(x, ris, type="s", xlab="Realizzazioni",
         ylab="F. di Ripartizione", 
         main = "V.C. Normale")
    points(x, ris, col=2, pch=16, cex =1.5)}
  return(ris)
}

# Principali quantili
Q_Norm<-function(media, sd){
  ris <- qnorm(c(.01, .25, .5, .75, .99), media, sd)
  return(ris)
}

# Dati casuali
R_Norm<-function(n, media, sd){
  ris <- rnorm(n, media, sd)
  return(ris)
}

# Funzione che restituisce tutti i valori
VC_Normale<-function(x=NULL, n, media, sd, plot=TRUE){
  fprob<-Fprob_Norm(x, media, sd, plot=plot)
  frip<-Frip_Norm(x, media, sd, plot=plot)
  va<-media
  var<-sd^2
  q<-Q_Norm(media, sd)
  r<-R_Norm(n, media, sd)
  # Stampa risultati
  cat("I valori della f. di prob. sono:", fprob, "\n")
  cat("I valori della f. di ripart. sono:", frip, "\n")
  cat("I principali quantili sono:", q, "\n")
  cat("Il valore atteso vale:", va, "\n")
  cat("La varianza vale", var, "\n")
  ris<-list(fprob, frip, va, var, q, r)
  return(ris)
}

# Esempio
xx<-VC_Normale(n=10, media = 50, sd = 7, plot = TRUE)
