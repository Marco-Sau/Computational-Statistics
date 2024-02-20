# ==== REGRESSIONE LINEARE ====================================================
rm(list=ls())

# Distribuzione dei dati (Blocco 1) -------------------------------------------        

#esempio di regressione lineare semplice con R

#leggiamo il file e carichiamolo in una dataframe 
advertising<-read.csv("24/advertising.csv",row.names="X")

#vediamo la struttura
str(advertising)

#vediamo il summary
summary(advertising)
names(advertising) <- c("TV","Radio","Newspaper","Sales")

#rendiamo attivo il dataframe
attach(advertising)
par(bg="white")
plot(TV, Sales, pch=16, col="blue")
plot(Radio, Sales, pch=16, col="blue")
plot(Newspaper, Sales, pch=16, col="blue")



# Retta di Regressione (Blocco 2) ---------------------------------------------
? lm
lm.st<-lm(Sales~TV)
plot(TV,Sales,pch=16,col="blue")

abline(lm.st,col="red",lwd=4)

segments(TV[1],Sales[1],TV[1],lm.st$fitted.values[1],
         col="orange",lty="dotted",lwd=3)
segments(TV[2],Sales[2],TV[2],lm.st$fitted.values[2],
         col="orange",lty="dotted",lwd=3)
segments(TV,Sales,TV,lm.st$fitted.values,lty="dotted",
         col="orange",lwd=3)


# Alcune funzioni ----------------
coef(lm.st) # coefficienti stimati
deviance(lm.st) # variabilità residua TSS
formula(lm.st) # equazione del modello
residuals(lm.st)# differenza tra osservazioni e valori stimati
model.matrix(lm.st) # matrice dei predittori
vcov(lm.st) 
# matrice delle varianze e covarianze dei coefficienti di regressione



# Retta Stimata (Blocco 3) ----------------------------------------------------

## Predictions
# con 10 osservazioni
set.seed(123)
x <- rnorm(10)
y <- 3 + x*2 + rnorm(10) # y = 3.16 + 2.63 * x
lm(y ~ x)->lll
summary(lll)

par(mfrow=c(1,3))
plot(x,y,pch=16,col="blue", main = "n = 10")
abline(lll,col="green", lty=2, lwd=4)
abline(a=3,b=2,col="red", lty=1, lwd=4)
legend("topleft", legend = c("Retta popolazione","Retta stimata"), 
       col = c("red","green"),
       lwd = 4, cex=.5)


# con 50 osservazioni
x <- rnorm(50)
y <- 3+x*2 + rnorm(50) # y = 3.02 + 2.05 * x
lm(y ~ x)->lll2
summary(lll2)

plot(x,y,pch=16,col="blue", main = "n = 50")
abline(lll2,col="green", lty=2, lwd=4)
abline(a=3,b=2,col="red", lty=1, lwd=4)
legend("topleft", legend = c("Retta popolazione","Retta stimata"), 
       col = c("red","green"),
       lwd = 4, cex=.5)


# con 1000 osservazioni
x <- rnorm(1000)
y <- 3+x*2 + rnorm(1000) # y = 3.02 + 2.05 * x
lm(y ~ x)->lll3
summary(lll3)

plot(x,y,pch=16,col="blue", main = "n = 50")
abline(lll3,col="green", lty=2, lwd=4)
abline(a=3,b=2,col="red", lty=1, lwd=4)
legend("topleft", legend = c("Retta popolazione","Retta stimata"), 
       col = c("red","green"),
       lwd = 4, cex=.5)


# Intervalli di Confidenza e Predizione ----------
par(mfrow=c(1,1))
x <- rnorm(10)
y <- 3 + (x * 2) + rnorm(10) # y = 3.02 + 2.05 * x

predict(lm(y ~ x)) # restituisce i valori stimati

# creiamo un nuovo dataset di tutti gli oggetti di cui vogliamo fare la predizione
new <- data.frame(x = seq(-3, 3, 0.05))
predict(lll, new, se.fit = TRUE) 
# lll è il modello
# new è il dataset
# se.fit = TRUE restituisce anche la deviazione standard

pred.w.plim <- predict(lll, new, interval = "prediction")
pred.w.clim <- predict(lll, new, interval = "confidence")
par(bg="white")
matplot(new$x, cbind(pred.w.clim, pred.w.plim[,-1]),
        lty = c(1,2,2,3,3), col=c(1,2,2,3,3),type = "l", ylab = "predicted y")

# new$x è il dataset su cui vogliamo fare la predizione
# cbind unisce i due dataset
# pred.w.clim è il dataset con gli intervalli di confidenza
# pred.w.plim è il dataset con gli intervalli di predizione



# Stima dei Modelli (Blocco 4) ------------------------------------------------
# Stimiamo i modelli
summary(m1<-lm(Sales~TV))
summary(m2<-lm(Sales~Radio))
summary(m3<-lm(Sales~Newspaper))

confint(m1)



# Predizioni sul dataset Boston (Blocco 5) ------------------------------------

library(MASS)
library(ISLR) #dataset

# Regressione lineare semplice
data("Boston")
?Boston

#fix(Boston)
names(Boston)
ds2 = Boston

names(ds2)[1] = "criminalita"
names(ds2)[2] = "residenziale"
names(ds2)[3] = "industriale"
names(ds2)[4:14] = c("fiume", "inquinamento", "stanze", "eta", 
                     "dist_centro", "dist_autostrade", 
                     "tassa_prop", "ins_stud", 
                     "immigrati", "poverta", 
                     "prezzo")

ds2$fiume
ds2$fiume = factor(ds2$fiume, labels = c("No","Yes"))


attach(ds2)
plot(poverta,prezzo)
lm.fit=lm(prezzo~poverta) #errore
lm.fit

summary(lm.fit)
names(lm.fit)
coef(lm.fit)
confint(lm.fit)

predict(lm.fit, data.frame(poveri=(c(5,10,15))), interval="confidence")
a = data.frame(poveri=(c(5,10,15)))
predict(lm.fit, a, interval="confidence")
predict(lm.fit,data.frame(poveri=(c(5,10,15))), interval="prediction")
?predict
predict(lm.fit)


plot(poverta,prezzo, type="n")
points(poverta[fiume=="Yes"],prezzo[fiume=="Yes"], 
       pch =10, col = "red")
points(poverta[fiume=="No"],prezzo[fiume=="No"], 
       pch =20, col = "blue")
abline(lm.fit,lwd=3,col="orange")



# Stima dei Modelli e funzione ANOVA (Blocco 6) --------------------------------

# Stimiamo i modelli 
summary(m0 <- lm(Sales ~ 1))    # modello trivial
summary(m1 <- lm(Sales ~ TV))
summary(m2 <- lm(Sales ~ Radio))
summary(m3 <- lm(Sales ~ Newspaper))
summary(m4 <- lm(Sales ~ TV + Radio))
summary(m5 <- lm(Sales ~ TV + Newspaper))
summary(m6 <- lm(Sales ~ Radio + Newspaper))
summary(m7 <- lm(Sales ~ TV + Radio + Newspaper)) # modello completo

confint(m4)

# ANOVA
? anova
anova(m1,m4,m7)



# Regressione Lineare Multipla (Blocco 7) --------------------------------------
# Regressione lineare multipla e selezione delle variabili

lm.fit=lm(prezzo~poverta+eta,data=ds2)
summary(lm.fit)
lm.fit=lm(prezzo ~ .,data=ds2)
summary(lm.fit)

? step 
step.fit<-step(lm.fit, direction="both",trace=TRUE)
summary(step.fit)

lm.fit0<-lm(prezzo~1,data=ds2)
step.fit2<-step(lm.fit0, scope = formula(lm.fit), direction="both",trace=TRUE)
summary(step.fit2)

pairs(ds2)


# Best subset selection --------------------
library(leaps) ##caricamento package leaps
mc<-lm(prezzo~., ds2) # modello completo

y<-ds2$prezzo
x<-model.matrix(mc)[,-1]
bss.mc<-leaps(x,y, method = "Cp")
bss.mc


which.min(bss.mc$Cp)
bss.mc$which[which.min(bss.mc$Cp),] 
# le variabili dalla 10 in poi vengono indicate con A, B, C....

bss.mc$label

#install.packages("faraway")
library(faraway)
Cpplot(bss.mc)

names(ds2) 
# il modello selezionato da BSS non include le variabili "industriale"
# ed "eta"

# NB Stesso risultato di Stepwise selection



# Dataset Credit (Blocco 8) ---------------------------------------------------
library(ISLR)
attach(Credit)
ls(Credit)
str(Credit)
summary(Credit)
? Credit
pairs(Credit)
pairs(Credit[,-1])
Credit<-Credit[,-1]


m0 <- lm(Balance ~ ., Credit[,-1])
summary(m0)


# Provare a stimare il modello effettuando selezione di variabili -------------
# Stepwise selection
step_fit <- step(m0, direction="both", trace=TRUE)
summary(step_fit)

step_fit2 <- step(m0, scope = formula(m0),
                  direction="both", trace=TRUE)
summary(step_fit2)

# Best subset selection
library(leaps)
y <- Credit$Balance
x <- model.matrix(m0)[,-1]
bss <- leaps(x, y, method = "Cp")
bss

which.min(bss$Cp)
bss$which[which.min(bss$Cp),]
bss$label
names(Credit)

# modello
m1 <- lm(Balance ~ Limit + Cards + Age + Student)
summary(m1)

confint(m1)

