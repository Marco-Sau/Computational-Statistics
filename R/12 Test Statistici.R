# ===== Lecture 20 ========================================================================
rm(list=ls())
#Test sul singolo valore medio

#La funzione e le opzioni più comuni sono:
# verifica mu=mu_0, H_1 bidirezionale
# > t.test(campione,mu=mu_0)
# H_1: mu > mu_0 (unidirezionale "maggiore di")
# > t.test(campione,mu=mu_0,alternative="g")
# H_1: mu < mu_0 (unidirezionale "minore di")
# > t.test(campione,mu=mu_0,alternative="l")
# livello critico alpha (in genere, alpha=0.05)
# > t.test(campione,mu=mu_0,conf.level=1-alpha)


# Dataset Orange Juice ---------------------------------------------------------

#install.packages("ISLR")
library(ISLR)
data(OJ)
summary(OJ)

mean(OJ$PriceCH)
mean(OJ$PriceMM)

head(OJ)

# Estraiamo un campione
PrezzoCH=OJ$PriceCH[1:30] 
PrezzoMM=OJ$PriceMM[1:30]

# Test sul valore medio con varianza nota --------------------------------------
# Esercizio slides ------------------------------------------------------------
# Test Bilaterale
# popolazione
media_pop <- 18.7
sd_pop <- 63
# campione
n <- 20
media_camp <- 15
alpha <- 0.01

# Ipotesi H0: mu = 18.7, H1: mu != 18.7
# Valori critici
z1 <- qnorm(alpha/2)
z2 <- - qnorm(alpha/2)
cat("Valori critici: ", z1, z2, "\n")

# Calcolo del valore di z
z <- (media_camp - media_pop)/(sd_pop/sqrt(n))
cat("Valore della statistica test: ", z, "\n")

# Decisione
if (z < z1 | z > z2) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}


# Test Unilaterale a Sinistra
# popolazione
media_pop <- 18.7
sd_pop <- 63
# campione
n <- 20
media_camp <- 15
alpha <- 0.01

# Ipotesi H0: mu = 18.7, H1: mu < 18.7
# Valore critico
z1 <- qnorm(alpha)
cat("Valore critico: ", z1, "\n")

# Calcolo del valore di z
z <- (media_camp - media_pop)/(sd_pop/sqrt(n))
cat("Valore della statistica test: ", z, "\n")

# Decisione
if (z < z1) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}

# Test sul valore medio con varianza incognita --------------------------------
# Esercizio lezione -----------------------------------------------------------
#? t.test

# test bidirezionale sulla media del campione PrezzoCH supponendo 
# che la media della popolazione sia 1.9
t.test(PrezzoCH,mu=1.9)


#? Results of t.test: explanation of the output
# 	One Sample t-test

# data:  PrezzoCH
# t = -3.3286, df = 29, p-value = 0.002384
## t is the t-statistic
## df is the degrees of freedom
## p-value is the p-value of the test
# alternative hypothesis: true mean is not equal to 1.9
## the alternative hypothesis is that the true mean is not equal to 1.9
# 95 percent confidence interval:
#  1.797213 1.875453
## the 95% confidence interval of the mean is 1.797213 1.875453
# sample estimates:
# mean of x
#  1.836333
## the sample mean is 1.836333

cat("Using t di Student", "\n")
cat("pt (-3.3286,29)", pt(-3.3286,29) * 2, "\n") # Questo è il p-value
cat("qt (0.025,29)", qt(0.025,29), "\n")
cat("qt (0.975,29)", qt(0.975,29), "\n")

cat("PrezzoMM supponendo che la media della popolazione sia 1.09")
t.test(PrezzoMM,mu=1.9)
# Il prezzo di MM è significativamente diverso da 1.9

cat("PrezzoCH supponendo che la media della popolazione sia 1.85")
t.test(PrezzoCH,mu=1.85)

cat("PrezzoCH supponendo che la media della popolazione sia 1.85, 
    test unilaterale a sinistra")
t.test(PrezzoCH,mu=1.85, alternative = "less")
cat("PrezzoCH supponendo che la media della popolazione sia 1.85, 
    test unilaterale a destra")
t.test(PrezzoCH,mu=1.85, alternative = "greater")

# Ripetere il test precedente con un campione di 100 elementi
# utilizzando quindi un campione di dimensione maggiore
cat("Utilizzando un campione di 100 elementi")
set.seed(123)
ind = sample(1:nrow(OJ), size = 100, replace = T)
ind

campione = OJ[ind,]

t.test(campione$PriceCH,mu=1.85)

pt(0.96148,99) * 2 #! This value should be equal to the p-value of the test
qt(0.025,99)
qt(0.975,99)


# Esercizio slides ------------------------------------------------------------
# test sulla media, bilaterale distribuzione non nota, varianza non nota, 
# CAMPIONE PICCOLO
# Campione
n <- 20
media_camp <- 70
sd_camp_corretto <- 25
# Popolazione
media_pop <- 61

# Ipotesi H0: mu = 61, H1: mu != 61
# Valori critici
t1 <- qt(alpha/2, df = n - 1)
t2 <- - qt(alpha/2, df = n - 1)
cat("Valori critici: ", t1, t2, "\n")

# Calcolo del valore di t
t <- (media_camp - media_pop)/(sd_camp_corretto/sqrt(n))
cat("Valore della statistica test: ", t, "\n")

# Decisione
if (t < z1 | t > z2) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}


# test sulla media, bilaterale distribuzione non nota, varianza non nota, 
# CAMPIONE GRANDE
# Campione
n <- 100
media_camp <- 70
sd_camp_corretto <- 25
# Popolazione
media_pop <- 61

# Ipotesi H0: mu = 61, H1: mu != 61
# Valori critici
t1 <- qt(alpha/2, df = n - 1)
t2 <- - qt(alpha/2, df = n - 1)
cat("Valori critici: ", t1, t2, "\n")

# Calcolo del valore di t
t <- (media_camp - media_pop)/(sd_camp_corretto/sqrt(n))
cat("Valore della statistica test: ", t, "\n")

# Decisione
if (t < z1 | t > z2) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}

# Valore soglia della Media ---------------------------------------------------
# Determinare il valore di soglia che permetta di discriminare tra
# i valori di p-value e di alpha, per capire quando rifiutare H_0
# e quando accettarla
cat("Mean price ch", mean(OJ$PriceCH), "\n")
cat("Mean price mm", mean(OJ$PriceMM), "\n")

cat("Mean sample price ch", mean(campione$PriceCH), "\n")
cat("Mean sample price mm", mean(campione$PriceMM), "\n")

cat("Test per verificare il valore di soglia per accettare o 
    rifiutare l'ipotesi nulla", "\n")

vector_mean <- c()
alpha <- 0.05
valori_soglia <- c()

for (i in seq(1.8, 1.95, 0.001)) {
  result <- t.test(PrezzoCH, mu = i)
  vector_mean <- c(vector_mean, result$p.value)
  if (result$p.value >= alpha/2 - 0.005 && result$p.value <= alpha/2 + 0.005) {
    valori_soglia <- c(valori_soglia, i)
  }
}

cat("Valori soglia per accettare o rifiutare l'ipotesi nulla", 
    valori_soglia, "\n")


par(bg = "white")
plot(seq(1.8, 1.95, 0.001), 
     vector_mean, 
     type = "l", 
     col = "blue", 
     xlab = "Mean (mu)", 
     ylab = "p-value")

abline(h = alpha/2, col = "red", lty = 2)
abline(v = valori_soglia, col = "green", lty = 2)
abline(v = mean(PrezzoCH), col = "black", lty = 2)

# Annotating threshold values on the plot
for (val in valori_soglia) {
  text(val, alpha/2, labels = round(val, 3), pos = 3, 
       cex = 0.8, col = "#ff2600")
}

legend("topright", 
       legend = c("p-value", "alpha/2", "Mean", "Threshold values"), 
       col = c("blue", "red", "black", "green"), 
       lty = c(1, 2, 2, 2), 
       cex = 0.8)


# Test sulla differenza tra medie con varianze note ----------------------------
# Esercizio slides ------------------------------------------------------------
# Campioni
n1 <- n2 <- 20
media_camp1 <- 271
media_camp2 <- 176
# sd_camp1 <- 200
# sd_camp2 <- 150
var_camp1 <- 4000
var_camp2 <- 3600
alpha <- 0.05

# Ipotesi H0: mu1 - mu2 = 0, H1: mu1 - mu2 != 0
# Valori critici
z1 <- qnorm(alpha/2)
z2 <- - qnorm(alpha/2)
cat("Valori critici: ", z1, z2, "\n")

# Calcolo del valore di z
z <- (media_camp1 - media_camp2)/sqrt(var_camp1/n1 + var_camp2/n2)
cat("Valore della statistica test: ", z, "\n")

# Decisione
if (z < z1 | z > z2) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}



# Test sulla differenza tra medie con varianze NON note ma supposte uguali ----
# Esercizio lezione -----------------------------------------------------------
# Null hypothesis: the mean of the population is 0
t.test(PrezzoCH,PrezzoMM,
       var.equal=TRUE) 
# il prezzo di MM e' mediamente piu' alto di quello di CH

t.test(PrezzoCH,PrezzoMM,
       var.equal=TRUE, 
       conf.level = 0.9)

# Null hypothesis: the mean of the population is -0.2, 
# cioe' il prezzo di MM e' mediamente piu' alto di quello di CH di 0.2
t.test(PrezzoCH,PrezzoMM,mu = -0.2,
       var.equal=TRUE)

t.test(PrezzoCH,PrezzoMM,mu = -0.2,
       alternative = "less",
       var.equal=TRUE)

# Cambiamo il livello di confidenza
t.test(PrezzoCH,PrezzoMM,var.equal=TRUE, 
       conf.level = 0.9)

# 100 elements sample
t.test(campione$PriceCH,campione$PriceMM,
       var.equal=TRUE,
       conf.level = 0.95)

# Null hypothesis: the mean of the population is < 0
t.test(campione$PriceCH,campione$PriceMM,
       var.equal=TRUE,
       alternative = "less", # test unilaterale a sinistra
       conf.level = 0.95)

# Null hypothesis: the mean of the population is -0.2
t.test(campione$PriceCH,campione$PriceMM, mu = -0.2,
       var.equal=TRUE, conf.level = 0.95)

t.test(campione$PriceCH,campione$PriceMM, mu = -0.2,
       var.equal=TRUE, alternative = "less",
       conf.level = 0.95)

# 30 elements sample
t.test(PrezzoCH,PrezzoMM,
       var.equal=TRUE,
       conf.level = 0.95)


# Variance of the two samples are not equal
# In this case we have more variability
t.test(PrezzoCH,PrezzoMM,
       var.equal=FALSE,
       conf.level = 0.95)

# Esercizio slides ------------------------------------------------------------
# Campioni
n1 <- n2 <- 20
media_camp1 <- 271
media_camp2 <- 176
sd_camp1 <- 200
sd_camp2 <- 150
# var_camp1 <- 4000
# var_camp2 <- 3600
alpha <- 0.05

# Ipotesi H0: mu1 - mu2 = 0, H1: mu1 - mu2 != 0
# Valori critici
t1 <- qt(alpha/2, df = n1 + n2 - 2)
t2 <- - qt(alpha/2, df = n1 + n2 - 2)
cat("Valori critici: ", t1, t2, "\n")

# Calcolo del valore di t
t <- (media_camp1 - media_camp2)/sqrt(sd_camp1^2/n1 + sd_camp2^2/n2)
cat("Valore della statistica test: ", t, "\n")

# Decisione
if (t < t1 | t > t2) {
  cat("Rifiuto H0")
} else {
  cat("NON Rifiuto H0")
}


# Test sulla varianza --------------------------------------------------------
# PrezzoCH
var(PrezzoCH) # 0.01097575 varianza campionaria corretta
var(OJ$PriceCH) # 0.01039783 

# H_0: sigma^2 = 0.010
# H_1: sigma^2 ≠ 0.010
# n = 30
# sigma^2~ = 0.01097575
# alpha = 0.05

n <- length(PrezzoCH)
Y <- ((30 - 1) * var(PrezzoCH)) / 0.010
cat("Valore della statistica di test:", Y, "\n")

# Confrontiamo con i valori critici della distribuzione chi quadro
q0025 <- qchisq(0.025, df = n - 1) # 16.04707
q0975 <- qchisq(0.975, df = n - 1) # 45.72229
cat("Valori critici della distribuzione chi quadro:", q0025, q0975, "\n")
# Non rifiuto l'ipotesi nulla

# media di una distribuzione chi-quadro = n - 1 gradi di liberta'
# qchisq(.5, df = n - 1) # 24.99579
pvalue <- (1 - dchisq(Y, 29)) / 2 # bidirezionale
cat("p-value", pvalue, "\n")

# la media e' 29, che sono i gradi di liberta'
pvalue <- 1 - dchisq(Y, 29) # unidirezionale a destra
cat("p-value unidirezionale a destra", pvalue, "\n")

# Funzione solo bi-direzionale
Test_varianza<-function(x, sigma2, alpha=0.05, alternative="two.sided"){
  n <- length(x)
  Y <- ((n-1) * var(x))/sigma2 # Statistica test
  alpha2 <- alpha/2
  q1 <-  qchisq(alpha2, n-1)
  q2 <-  qchisq(1-alpha2, n-1)
  if(Y < q1 | Y > q2) cat("Rifiuto H0", "\n")
  else cat("Non rifiuto H0", "\n")
  cat("La statistica test e pari a", Y, "\n")
  if(Y >= (n-1))
    pvalue <- (1 - dchisq(Y, n-1))/2
  else  pvalue <- (dchisq(Y, n-1))/2 
  cat("il p-value e pari a", pvalue, "\n")
  ris <- c(Y, q1, q2, pvalue)
  cat("La statistica test e pari a", Y, "\n",
      "il p-value e pari a", pvalue, "\n",
      "i valori critici sono", q1, q2, "\n")
  return(ris)
}

Test_varianza(PrezzoCH, 0.010)

# Funzione completa
Test_varianza <- function(x, sigma2, alpha = 0.05, alternative = "two.sided") {
  n <- length(x)
  Y <- ((n - 1) * var(x)) / sigma2 # Statistica test
  q_lower <- qchisq(alpha, n - 1)
  q_upper <- qchisq(1 - alpha, n - 1)
  
  if (alternative == "two.sided") {
    if (Y < q_lower || Y > q_upper) cat("Rifiuto H0", "\n")
    else cat("Non rifiuto H0", "\n")
    if(Y >= (n-1))
      pvalue <- (1 - dchisq(Y, n-1))/2
    else  pvalue <- (dchisq(Y, n-1))/2 
    
  } else if (alternative == "less") {
    if (Y < q_lower) cat("Rifiuto H0", "\n")
    else cat("Non rifiuto H0", "\n")
    pvalue <- pchisq(Y, n - 1)
    
  } else if (alternative == "greater") {
    if (Y > q_upper) cat("Rifiuto H0", "\n")
    else cat("Non rifiuto H0", "\n")
    pvalue <- 1 - pchisq(Y, n - 1)
    
  } else {
    stop("Alternative deve essere 'two.sided', 'less', o 'greater'")
  }
  
  cat("La statistica test è pari a", Y, "\n",
      "il p-value è pari a", pvalue, "\n",
      "i valori critici sono", q_lower, q_upper, "\n")
  
  return(c(Y, q_lower, q_upper, pvalue))
}

Test_varianza(PrezzoCH, 0.010)



# Test sul rapporto tra varianze -------------------------------------------
? var.test
var.test(PrezzoCH,PrezzoMM,
         alternative="two.sided")

var.test(campione$PriceCH,campione$PriceMM,
         alternative="two.sided")

var.test(campione$PriceCH,campione$PriceMM,
         alternative="less")




# Test sulla correlazione -----------------------------------------------------
? cor.test
# Correlation between the two samples
cor(PrezzoCH,PrezzoMM)

# Degree of freedom
# The degrees of freedom in this case are 28
# because it's like we are working
# in bivariate context
# (x_i, y_i) i = 1, ..., 30
# insted of
# (x_i),(y_i) i = 1, ..., 30

# Il valore critico non si puo' cambiare
cor.test(PrezzoCH,PrezzoMM, 
         alternative="two.sided")
# La correlazione e' significativamente diversa da 0 nella popolazione

cor.test(PrezzoCH,PrezzoMM, 
         alternative="greater")




# Test sulla proporzione di successi ------------------------------------------
names(OJ)
table(OJ$Purchase)

propCH = 653 / (653 + 417)
cat("Proportion of CH", propCH, "\n")
propMM = 1 - propCH
cat("Proportion of MM", propMM, "\n")

set.seed(123)
# Extract a sample of 200 elements from OJ
ind2<-sample(1:nrow(OJ), size=200, replace=T)
# ind2
campione<-OJ[ind2,]

summary(campione)

# Proportions in the samples
propCHcamp = 127/200
cat("Proportion of CH in the sample", propCHcamp, "\n")
propMMcamp = 1 - propCHcamp
cat("Proportion of MM in the sample", propMMcamp, "\n")

dCH = c(127,200)
dMM = c(73,200)

# ? prop.test
prop.test(127, 200) # test sulla proporzione di successi
# p = 0.5, alternative p ≠ 0.5

# La proporzione campionaria e uguale alla proporzione nella popolazione?
prop.test(127, 200, p = propCH)

prop.test(127, 200, p = propCH, alternative = "g")


# Test sulla differenza tra proporzioni --------------------------------------
# Confrontare due proporzioni
vnum = c(127,73) # numeratori
vden = c(200,200) # denominatori

# null hypothesis: p1 = p2
# alternative hypothesis: p1 ≠ p2
prop.test(vnum,vden)

# Ha senso solo greater, perche' la differenza e' positiva
# quindi la differenza e' maggiore di 0
prop.test(vnum,vden, alternative = "g")

# change the proportion value
# 0.6 and 0.4 because prop1 = 0.635 and prop2 = 0.365
# L'ipotesi nulla e' che la differenza sia uguale a 0.2
# L'ipotesi alternativa e' che la differenza sia maggiore di 0.2
prop.test(vnum,vden, alternative = "g", p = c(0.6,0.4)) # difference 0.2
#! Even if it's one sided test, the result says two.sided

prop.test(cbind(dCH,dMM),alternative="greater",
          correct=FALSE)










###############################################################################
# MATERIALE AGGIUNTIVO CHE NON FA PARTE DEL PROGRAMMA

table(campione$Purchase,campione$STORE)
? chisq.test
chisq.test(table(campione$Purchase,campione$STORE),correct=FALSE)

##### Distribuzioni

###### NORMAL DISTRIBUTION ######

mean <- 10
std_dev <- 3

limit_inf <- mean - std_dev*5
limit_sup <- mean + std_dev*5

x_values <- seq(limit_inf,limit_sup,0.05)
density_values <- dnorm(x_values, mean = mean, sd = std_dev) 
# dt returns the value of the probability density function (pdf)
density_values
plot(x_values, density_values)
plot(x_values, density_values, type = "l")

# sum of area under the curve
integrate(function(x) dnorm(x,mean,std_dev), -Inf, Inf)

integrate(function(x) dnorm(x,mean,std_dev), 5, 12)


# benchmark points
abline(v = mean, col = "red")
abline(v = mean - std_dev, col = "green", lty=2)
abline(v = mean + std_dev, col = "green", lty=2)

###### Z DISTRIBUTION ######


y1 <- rnorm(10000, mean = 10, sd = 3)
y2 <- rnorm(10000, mean = -3, sd = 0.5)

hist(y1, probability = TRUE, xlim = c(-5,20), ylim = c(0,1), col = "red")
hist(y2, probability = TRUE, add = TRUE, col = "blue")


# Z = (Y - mean)/sd
z1 = (y1 - mean(y1))/sd(y1)
mean(z1)
sd(z1)

z2 = (y2 - mean(y2))/sd(y2)
mean(z2)
sd(z2)

hist(z1, probability = TRUE, add = TRUE, col = scales::alpha("green", 0.4))
hist(z2, probability = TRUE, add = TRUE, col = scales::alpha("pink", 0.4))



###### CHI2 DISTRIBUTION ######

degrees_freedom <- 5

x_values <- seq(0,40,0.05)
density_values <- dchisq(x_values, df = degrees_freedom)
plot(x_values, density_values)
plot(x_values, density_values, type = "l")

# sum of area under the curve
integrate(function(x) dchisq(x,degrees_freedom), 0, Inf)
integrate(function(x) dchisq(x,degrees_freedom), 10, 20)


exp_value <- degrees_freedom
std_dev <- degrees_freedom

# benchmark points
abline(v = exp_value, col = "red")
abline(v = exp_value - std_dev, col = "green", lty=2)
abline(v = exp_value + std_dev, col = "green", lty=2)


# on a practical point of view

z <- rnorm(10000, mean = 0, sd = 1)

chi_5 <- (rnorm(10000, mean = 0, sd = 1)^2 +
            rnorm(10000, mean = 0, sd = 1)^2 +
            rnorm(10000, mean = 0, sd = 1)^2 +  
            rnorm(10000, mean = 0, sd = 1)^2 +  
            rnorm(10000, mean = 0, sd = 1)^2)


hist(z, breaks = 40, xlim=c(-5,30), probability = TRUE)
hist(chi_5, breaks = 40, add=TRUE, probability = TRUE, col = "red")


hist(chi_5, breaks = 40, probability = TRUE)
lines(density(chi_5,adjust = 2))

abline(v = mean(chi_5), col = "red")


#### T student

degrees_freedom <- 5

z <- rnorm(10000, mean = 0, sd = 1)
chi <- rchisq(10000, df= degrees_freedom)
t <- z / sqrt(chi/degrees_freedom)
hist(t, breaks = 40, probability = TRUE)
lines(density(t,adjust = 2))


#### F Fisher
degrees_freedom1 <- 10
degrees_freedom2 <- 12

f_fisher <- (rchisq(10000, df= degrees_freedom1)/
               degrees_freedom1)/
  (rchisq(10000, df= degrees_freedom2)/
     degrees_freedom2)

hist(f_fisher, breaks = 40, probability = TRUE)


x_values <- seq(0,10,0.05)
density_values <- df(x_values, df1 = degrees_freedom1, df2 = degrees_freedom2)
plot(x_values, density_values)
plot(x_values, density_values, type = "l")

####### Terorema Limite Centrale

#---------------------------------------------------------------

# For any Normal distribution specific proportions occurs
# within certain distances from the mean:
# - 50% of population falls between mean +/- 0.674*sd
# - 95% of population falls between mean +/- 1.960*sd
# - 99% of population falls between mean +/- 2.576*sd

mean1 <- 20
std_dev1 <- 0.5

mean2 <- 10
std_dev2 <- 3

(n1a <- mean1 - 1.960*std_dev1)
(n1b <- mean1 + 1.960*std_dev1)
integrate(function(x) dnorm(x,mean1,std_dev1), n1a, n1b)

(n2a <- mean2 - 1.960*std_dev2)
(n2b <- mean2 + 1.960*std_dev2)
integrate(function(x) dnorm(x,mean2,std_dev2), n2a, n2b)


#---------------------------------------------------------------

# Integrate every time is too difficult

## Case Z
mean <- 0
std_dev <- 1

x_values <- seq(-std_dev*5,std_dev*5,0.05)
density_values <- dnorm(x_values, mean = mean, sd = std_dev)
plot(x_values, density_values)
plot(x_values, density_values, type = "l")
abline(v = mean, col = "red")


# So we can define a range [-z, z] for each probability
# where z is called "standard score"
# example 95% z = 1.96

integrate(function(x) dnorm(x,0,1), -1.960, +1.960)


# generalize Z to X
# P(-z < Z < z) = 1-alpha
# Z transformation -> Z = (X-mu) / sigma
# P(-z < (X-mu) / sigma < z) = 1-alpha
# P(-z*sigma < (X-mu) < z*sigma) = 1-alpha
# P(mu -z*sigma < X < mu + z*sigma) = 1-alpha





#---------------------------------------------------------------


N <- 20
n <- 5

set.seed(1)
pop <- sample(N/4,N,replace = TRUE)
barplot(table(pop))
mean(pop)
abline(v = mean(pop), col="red")

possible_samples <- combn(1:N, n)
dim(possible_samples)
possible_samples[,1:10]

all_means <- apply(possible_samples, 2, function(X) mean(pop[X]))

hist(all_means, probability = TRUE, breaks = 20)
lines(density(all_means, adjust = 2))
abline(v = mean(pop), col = "red")


mean(pop)
mean(all_means)

var(pop)/n
var(all_means)


plot(0,xlim=c(1,5),ylim=c(0,2),col="white")

sd_samples <- rep(NA,14)
names(sd_samples) <- 2:15
for(n in 2:15){
  possible_samples <- combn(1:N, n)#expand.grid(rep(list(1:N),n))
  all_means <- apply(possible_samples, 2, function(X) mean(pop[X]))
  lines(density(all_means, adjust = 2), col=rainbow(14)[n-1])
  sd_samples[as.character(n)] <- sd(all_means)
}

plot(2:15,1.96*sd_samples*2)







