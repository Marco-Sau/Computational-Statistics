# Definire una v.c. che riproduce i risultati 
# dell'esperimento consistente nel lanciare
# simultaneamente 2 dadi equilibrati e 2 monete regolari.
# Il punteggio finale è pari alla somma dei punteggi ottenuti
# nel lancio dei dadi meno una penalizzazione di 3 punti 
# per ogni Testa.
# Definire:
# a) la funzione di probabilità
# b) la funzione di ripartizione
# e calcolare:
# d) il valore atteso
# e) lo scarto quadratico medio
# f)  rappresentare graficamente i risultati ottenuti 
#     ipotizzando di ripetere l'esperimento 
#     100 volte


# Esperimenti
## Risultato di un singolo esperimento
set.seed(123)
# ? sample
sum(
  sample(1:6, size= 2, replace = T) +
    sample(c(-3,0), size= 2, replace = T)
)

## Salviamo i risultati del lancio del dadi
z <- vector()
for(i in 1:6)
  for(j in 1:6)
    z <- c(z, sum(i+j))
z

## Salviamo i risultati del lancio delle monete
r <- c(-3,0)
k <- vector()
for(i in 1:2)
  for(j in 1:2)
    k <- c(k, sum(r[i]+r[j]))
k

## Sommiamo i risultati
xx <- vector()
for(i in 1:length(z))
  for(j in 1:length(k))
    xx <- c(xx, z[i]+k[j])
xx

# Funzione di probabilità
## Ordiniamo i risultati
xx<-sort(xx)
xx

## Calcoliamo la probabilità
round(table(xx)/length(xx),4)

x<-unique(xx)
px<-as.vector(round(table(xx)/length(xx),4))

plot(x, px, type="h")


# Funzione di ripartizione
Fx<-cumsum(px)

plot(x, Fx, type="p")


# Valore atteso
Ex <- sum(x*px)
Ex

# Varianza
VarX <- mean((x - Ex)^2)
VarX

# Scarto quadratico medio
sqmX<-sqrt(VarX)
sqmX


# Ripetiamo l'esperimento 100 volte
ris100<-rep(0,100)
for(i in 1:100)
  ris100[i]<-sum(
  sample(1:6, size= 2, replace = T) +
    sample(c(-3,0), size= 2, replace = T))

ris100

plot(ris100, type="h")
