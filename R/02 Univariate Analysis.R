# ====================== UNIVARIATE ANALYSIS ================================== 
#### ==== Lecture 09 ==========================================================
## 2.1 Data Vectors @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#rm(list = ls()) # delete all the objects in the memory

# Install the packages 
#install.packages("lubridate")
library(lubridate) # upload the entire package
#require(lubridate) # upload the package partially

### Data types: Date and time types -------------------------------------------
now()
current_time <- now()
class(current_time)
# The Posixct class records a moment in time like
# the number of seconds since the beginning of 1970,
# if the value in negative it means that the date is before 1970

as.numeric(current_time)
# current time
# cf. ?POSIXct

month(current_time, 
      label=TRUE) # show the levels
month(current_time, label=F)   
# what month?
## [1] May
## 12 Levels: Jan < Feb < Mar < Apr < May < Jun < Jul < ... < Dec
# Other similar functions include Year (year), 
# Day (day), Hour (now), Minute (minute), and so on

mode(current_time) # mode of the object
str(current_time) # structure of the object

# In addition to the use of `Now`, it is possible to create times in other ways. 
 # One of these modalities is through the function `parse_date_time`
? parse_date_time
x <- c("09-01-01", "09-01-02", "09-01-03")
x
mode(x)

parse_date_time(x, "ymd", tz="CET")
parse_date_time(x, "y m d", tz = "Europe/Rome")
parse_date_time(x, "%y%m%d") # UTC time zone

# same as now function
# useful to calculate the computational time
u <- Sys.time()
u

#Calculate the difference between two dates
u - current_time

# switch the time zone
attr(u, "tzone") <- "UTC"
u

x <- "15-Feb-2013 07:57:34"
y <- parse_date_time(x, "dbYHMS") # day and month are db to avoid confusion
y
year(y)                                 

# working with dates
now() - days(1)
now() - hours(24)

# Create a data object
# sequence of days
days_sequence <- seq(dmy("01-01-2020"), dmy("31-12-2021"), by = "days")
days_sequence

# sequence of months
month_sequence <- seq(ymd("2020-01-01"), ymd("2020-12-31"), by = "months")
month_sequence

mode(days_sequence)
str(days_sequence)
length(days_sequence)

# Exercise: Days from my birhday
my_birthday <- ymd("1981-12-03")
my_birthday
current_day <- Sys.Date()
current_day
days_from_my_birthday <- as.numeric(current_day - my_birthday)
days_from_my_birthday

# Other useful functions
# Any, All, Which and % in % functions for vectors

# runif generates random numbers from a uniform distribution
x<-round(runif(10)*10,0) 
any(x == 0)
all(x <= 10)
which(x > 5)
x %in% 0


### Data types: Factors -------------------------------------------------------
#setwd("~/Downloads/_StatComp")

#install.packages("readr")
library(readr)

# Import the data-set
AcquistiOnLine <- read_csv("09/AcquistiOnLine.csv")

summary(AcquistiOnLine)

AcquistiOnLine <- AcquistiOnLine[,-1] # delete the first column
summary(AcquistiOnLine)

# With the function `str` we can see the structure of the data
str(AcquistiOnLine)

# Convert the variables into factors
AcquistiOnLine$Genere <- as.factor(AcquistiOnLine$Genere)
AcquistiOnLine$Genere

str(AcquistiOnLine)

# Upload the data-set permanently on the memory
attach(AcquistiOnLine)

# Convert the variables into factors into the attached data-set
AcquistiOnLine$Area_geografica <- as.factor(Area_geografica)
AcquistiOnLine$Area_geografica
str(AcquistiOnLine)
# Overwrite the variable in the data-set to be a factor
AcquistiOnLine$Area_geografica <- Area_geografica

# Create a qualitative ordered variable
#? as.ordered
Livello_istruzione <- factor(Livello_istruzione, 
                        levels = c("Diploma","Laurea", "Master", "Dottorato"),
                           ordered = TRUE) 
Livello_istruzione
AcquistiOnLine$Livello_istruzione <- Livello_istruzione
str(AcquistiOnLine)

Acquisto_online

# Convert a logical variable into a factor
Acquisto_online[Acquisto_online == FALSE] <- "No"
Acquisto_online[Acquisto_online == TRUE] <- "Si"
Acquisto_online
Acquisto_online <- factor(Acquisto_online, levels = c("No", "Si"))
Acquisto_online

str(AcquistiOnLine)
AcquistiOnLine$Acquisto_online <- Acquisto_online

str(AcquistiOnLine)



## 2.3 Numeric Summaries @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Central trend indicators **************************************************
#### mean ---------------------------------------------------------------------
mean(Prezzo_acquisto)

# Trimmed mean
mean(Prezzo_acquisto, trim = 0.05) # truncated mean
# Exclude 5 percent of the observations most far from the mean

# Weighted mean
# Media of the weighted purchase price for the number of pages visited
mean(Prezzo_acquisto*(Numero_pagine_visitate/sum(Numero_pagine_visitate)))

# Calculating the weighted mean of 'Prezzo_acquisto' 
# using 'Numero_pagine_visitate' as weights
# GPT

# Calculate the weighted mean
weighted_mean <- weighted.mean(Prezzo_acquisto, Numero_pagine_visitate)

# Print the result
print(weighted_mean)



#### Median -------------------------------------------------------------------
# median means the value that divides the sample in half
median(Prezzo_acquisto)
median(Numero_pagine_visitate)

# calculate for the qualitative variables
as.numeric(Livello_istruzione) # first convert the variable into numeric
median(as.numeric(Livello_istruzione))


#### Quantiles ----------------------------------------------------------------
? quantile
quantile(Prezzo_acquisto)
quantile(Prezzo_acquisto, probs = c(0.01,0.05,.25,.75,.95,.99))

? fivenum
# Useful to build the boxplot
fivenum(Prezzo_acquisto)
#edit(fivenum)


#### Mode ---------------------------------------------------------------------
Numero_pagine_visitate
x<-Area_geografica

# Using the table function
table(Area_geografica)
moda <- max(table(Area_geografica))
moda

# Manually calculate the mode
#! Doesn't work with qualitative variables
moda<-function(x){
  flag<-0
  if(is.factor(x)){
    flag<-1
    x.old<-unique(x)
    x<-as.numeric(x)
  } 
  v<-unique(x)
  somme<-list()
  for(i in length(x))
    somme[[i]]<-sum(x[x == v[i]])
  ris<-v[which.max(somme)]
  if(flag == 1) ris<-x.old[ris]
  return(ris)
}

# moda(Numero_pagine_visitate)
# moda(Area_geografica) # doesn't work with qualitative variables

# my solution
#?sapply
sum_elements <- function(x, value) { # x is a vector, value is a scalar
  sum(x == value)
}

# GPT
calculate_mode <- function(column) {
  unique_values <- unique(column)
  cat("Unique values:", unique_values, "\n")
#    frequencies <- sapply(unique_values, function(x) sum(column == x))
  frequencies <- sapply(unique_values, sum_elements, x = column)
  cat("Frequencies:", frequencies, "\n")
  max_freq <- max(frequencies)
  cat("Max frequency:", max_freq, "\n")
  mode_values <- unique_values[frequencies == max_freq]
  return(mode_values)
}

calculate_mode(Area_geografica)
calculate_mode(Numero_pagine_visitate)


#### ==== Lecture 10 ==========================================================
### Spread indicators *********************************************************
# Dispersion indicators
# (Indici di Variabilita')
#### Range --------------------------------------------------------------------
? range
range(Prezzo_acquisto)
max(Prezzo_acquisto)-min(Prezzo_acquisto)


#### Interquartile range ------------------------------------------------------
? IQR
IQR(Prezzo_acquisto)


#### Variance -----------------------------------------------------------------
? var
var(Prezzo_acquisto) # sample variance
mean((Prezzo_acquisto - mean(Prezzo_acquisto))^2) # variance of the population

# multiply by (n-1)/n to get the population variance
# population variance
var(Prezzo_acquisto)*(length(Prezzo_acquisto)-1)/length(Prezzo_acquisto)

#### Standard deviation -------------------------------------------------------
? sd
sd(Prezzo_acquisto) # Sample standard deviation
# If I know the population mean, I can calculate
# the population standard deviation

# population standard deviation
sqrt(mean((Prezzo_acquisto - mean(Prezzo_acquisto))^2)) 


#### Z-score ------------------------------------------------------------------
# Is the number of standard deviations that a given value x 
# is above or below the mean
# Neutralize the scale differences between variables
# mean = 0, sd = 1

# Manually build the z-score
z_score<-function(x){
  (x - mean(x)) / sd(x)
}
z_score(Prezzo_acquisto)

# Using the scale function
pas <- scale(Prezzo_acquisto) # pas = prezzo di acquisto scalato
pas # numeric vector

sum(pas)
mean(pas)
sd(pas)

# Why is important to standardize?
# z-score (scale)
  # in una distribuzione campanulare:
  # il 68% dei casi avrà uno z-score compreso tra -1 e 1 
  #(non più di 1 deviazione standard dalla media), 
  # il 95% sarà compreso tra -2 e 2 
  # e il 99.7% sarà compreso tra -3 e 3.
  # Inoltre, per qualsiasi distribuzione (non solo "campanulare"), 
  # il teorema di Chebyshev ci dice che la proporzione di valori 
  # con uno z-score assoluto superiore a k non è superiore a 1/k^2
  # to check the distribution of the variable a plot can be used

plot(1:100, sort(pas))
# In this example the distribution is not "campanulare"

# let's focus only on those who has buy something
pas2 <- scale(Prezzo_acquisto[Prezzo_acquisto > 0])
mean(pas2)
sd(pas2)

plot(1:length(pas2), sort(pas2))

# We need to use the histogram graph
hist(pas)
hist(pas2) 
hist(pas2, breaks = 20)


#### Chebyshev Inequality -----------------------------------------------------
# Ci permette di individuare la proporzione di valori che si trovano
# a una distanza k dalla media
# This inequality is used to estimate the minimum amount 
# of data that is within a certain number of standard deviations from the mean
# minimal proportion of values that are at a distance k from the mean
# Use case: population that want to pay between 10 and 20

# k = 2
mean(Spesa_media_mensile) # 76.3609
sd(Spesa_media_mensile) # 12.7419

# mu +- k*sd
mean(Spesa_media_mensile) + 2*sd(Spesa_media_mensile) # 101.8447
mean(Spesa_media_mensile) - 2*sd(Spesa_media_mensile) # 50.8771
1 - 1/2^2
# 75% of the population is willing to pay between 51 and 102 euros

# create a function
cheb<-function(x, k){
  min <- mean(x) - k*sd(x)
  max <- mean(x) + k*sd(x)
  value <- 1 - 1/k^2
  return(c(min, max, value))
}

cheb(Spesa_media_mensile, 2) # 0.75


#### Coefficient of variation -------------------------------------------------
# variabilita della distribuzione tenendo conto della media aritmetica
# Indice di variablitita' relativa (Useful to compare distributions)
# CV (calcolare) coefficiente di variazione
# CV = sd/|mean|
# sqm / media in valore assoluto
# Absolute value divided by the mean
sd(Spesa_media_mensile)/abs(mean(Spesa_media_mensile)) * 100 # 0.1668 (16%)

# create a function
cv<-function(x){
  sd(x)/abs(mean(x)) * 100
}

cv(Spesa_media_mensile)


#### Median absolute deviation ------------------------------------------------
# la mediana e' piu' robusta rispetto alla media
# mean absolute deviation from the median
# Why do we have to use this?
# MAD median absolute deviation
? mad
mad(Spesa_media_mensile)
var(Spesa_media_mensile)
range(Spesa_media_mensile)
median(Spesa_media_mensile)


# (Indici di mutabilita')
#### Gini coefficient ---------------------------------------------------------
# Qualitative data
# Massima omonogeneità (0) tutta la popolazione ha la stessa caratteristica
# Massima eterogeneità (1) le caratteristiche sono distribuite in modo uniforme
gini<-function(x){
  if(is.factor(x) == FALSE){
    paste("La variabile deve essere di tipo factor")
    stop()
  }
  f <- as.numeric(table(x)/length(x))
  G <- 1 - sum(f^2)
  G_rel <- G * (length(f)/(length(f)-1))
  return(c("G"=G, "G_rel"=G_rel))
}

gini(Area_geografica)
table(Area_geografica)

gini(Livello_istruzione)
table(Livello_istruzione)


# GPT
calculateGiniIndices <- function(frequencies) {
  k <- length(frequencies)
  total <- sum(frequencies)
  proportions <- frequencies / total
  
  # Gini Index
  G <- 1 - sum(proportions^2)
  
  # Maximum Gini Index
  Gmax <- (k - 1) / k
  
  # Normalized Gini Index
  G_star <- G / Gmax
  
  # Return a list containing all three indices
  return(list(G = G, Gmax = Gmax, G_star = G_star))
}
calculateGiniIndices(table(Area_geografica))



### Shape indicators **********************************************************
# Studieremo la simmetria e la curtosi
# Simmetrica significa che la distribuzione è uguale a destra 
# e a sinistra della media
# Curtosi è una misura di quanto le code di una distribuzione 
# differiscono da quelle di una distribuzione normale

#### Skewness -----------------------------------------------------------------
hist(Prezzo_acquisto) # asimmetrica positiva
hist(-Prezzo_acquisto) # asimmetrica negativa
hist(rnorm(10000)) # simmetrica

# Fisher's 
# Value = 0: simmetrica
# Value > 0: asimmetrica positiva
# Value < 0: asimmetrica negativa
asim_fisher <- function(x){
  if(is.numeric(x) == FALSE){
    paste("The input is not numeric")
    stop()
  }
  z <- scale(x)
  a <- mean(z^3)
  return(a)
}

asim_fisher(Prezzo_acquisto)
asim_fisher(rnorm(10000)) # Expected value = 0


#### Kurtosis -----------------------------------------------------------------
# Kurtosis is a measure of whether the data are heavy-tailed or light-tailed
# platikurtic (light-tailed) or leptokurtic (heavy-tailed)
forma_fisher <- function(x){
  if(is.numeric(x) == FALSE){
    paste("The input is not numeric")
    stop()
  }
  z <- scale(x)
  a <- mean(z^3)
  k <- mean(z^4)-3
  return(c("Asimmetria" = a, "Curtosi" = k))
}

forma_fisher(Prezzo_acquisto) # more flat compare a normal distribution
forma_fisher(rnorm(10000)) # Expected values = 0, platikurtic



### Vieweing the shape of a data set
#### Stripchart ---------------------------------------------------------------
stripchart(Prezzo_acquisto)
stripchart(Prezzo_acquisto ~ Area_geografica)


#### Stem and leaf plot -------------------------------------------------------
stem(Prezzo_acquisto)


#### Histogram ----------------------------------------------------------------
?hist
hist(Spesa_media_mensile)

interval <- c(min(Spesa_media_mensile), 50, 75, 90, max(Spesa_media_mensile))
hist(Spesa_media_mensile, breaks = interval)
# probability means that the sum of the bars is 1
hist(Spesa_media_mensile, breaks = interval, probability = FALSE)

hist(Spesa_media_mensile, breaks =50)
hist(rnorm(1000), breaks =50)


#### Density plot -------------------------------------------------------------
# distribuzione di probabilità teorica 
# variabili discrete -> funzione di probabilità
# variabili continue -> funzione di densità di probabilità
density(Spesa_media_mensile)
plot(density(Spesa_media_mensile))

  # Density
# -	Argomenti  
# -	xlim , ylim, xlab, ylab, main 
# -	pch (simboli) 
# - cex (dimensione)  
# - col
# - lwd, lty = "blank", "solid", "dashed", "dotted", "dotdash", etc.
# - bty  "box type" = "o", "l", "7", "c", "u", or "]


#### Histogram and density ----------------------------------------------------
hist(Spesa_media_mensile, probability = TRUE)
lines(density(Spesa_media_mensile), col=2)

hist(Spesa_media_mensile, probability = TRUE, ylim=c(0, 0.05))
lines(density(rnorm(100, 
                    mean(Spesa_media_mensile), 
                    sd(Spesa_media_mensile))), col=2)

hist(Spesa_media_mensile, probability = TRUE, ylim=c(0, 0.05), breaks = 18 )
lines(density(rnorm(100, 
                    mean(Spesa_media_mensile), 
                    sd(Spesa_media_mensile))), col=2)

names(AcquistiOnLine)

#### ==== Lecture 11 ==========================================================
#### Boxplots -----------------------------------------------------------------
? boxplot

# with Livello_istruzione doen't make sense
boxplot(Livello_istruzione)

boxplot(Spesa_media_mensile)
boxplot(Spesa_media_mensile, horizontal = T)

par(mfrow=c(1,2))
boxplot(Spesa_media_mensile[Genere=="Femmina"], 
        main = "Femmina")
boxplot(Spesa_media_mensile[Genere=="Maschio"],
        main="Maschio")
par(mfrow=c(1,1))
boxplot(Spesa_media_mensile~Genere, col=c(2,3))
boxplot(Spesa_media_mensile~Livello_istruzione, col=2:5)


#### Quantile graph: qqnorm ---------------------------------------------------
? qqnorm
# Confronta le distribuzioni in temrmini di quantili
qqnorm(Spesa_media_mensile)

# Se i dati sono normali, i punti dovrebbero essere allineati sulla retta
qqline(Spesa_media_mensile)


# Standardize the data
# The graph is the same, but now we have the same scale
qqnorm(scale(Spesa_media_mensile))
qqline(scale(Spesa_media_mensile))


#### Quantile graph: qqplot ---------------------------------------------------
? qqplot
qqplot(Spesa_media_mensile,Prezzo_acquisto)

# If the arguments don't have the same length, doens't work
qqplot(Spesa_media_mensile[Genere=="Maschio"],
     Spesa_media_mensile[Genere=="Femmina"])
# abline(lsfit(Spesa_media_mensile[Genere=="Maschio"],
#      Spesa_media_mensile[Genere=="Femmina"]))


# lsfit() is used to fit a simple linear regression model
# lsfit(x, y, intercept = TRUE, ...)
# in our case x = Spesa_media_mensile[Genere=="Maschio"]
# and y = Spesa_media_mensile[Genere=="Femmina"]
abline(lsfit(
as.numeric(quantile(Spesa_media_mensile[Genere=="Maschio"], 
                    probs = seq(.01,.99,0.01))),
as.numeric(quantile(Spesa_media_mensile[Genere=="Femmina"], 
                    probs = seq(.01,.99,0.01))),
))




## 2.4 Categorical data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# creazione di un oggetto factor
summary(AcquistiOnLine)  

# Creazione di un oggetto factor
Prezzo_acquisto_cat <- Prezzo_acquisto 
Prezzo_acquisto_cat[Prezzo_acquisto==0] <- 
  "01_Nessun acquisto"
Prezzo_acquisto_cat[Prezzo_acquisto > 0 & Prezzo_acquisto <= 25] <- 
  "02_Max 25 Euro"
Prezzo_acquisto_cat[Prezzo_acquisto > 25 & Prezzo_acquisto <= 50] <- 
  "03_tra 25 e 50 Euro"
Prezzo_acquisto_cat[Prezzo_acquisto >  50] <- 
  "04_Più di 50 Euro"

Prezzo_acquisto_cat<-factor(Prezzo_acquisto_cat, ordered = T)

AcquistiOnLine$Prezzo_acquisto_cat <- Prezzo_acquisto_cat

summary(Prezzo_acquisto_cat)
summary(AcquistiOnLine)

#### table --------------------------------------------------------------------
# frequenze assolute
table(Prezzo_acquisto_cat)
# frequenze relative
table(Prezzo_acquisto_cat)/length(Prezzo_acquisto_cat)

# distribuzioni doppie
table(Prezzo_acquisto_cat,Genere)
table(Prezzo_acquisto_cat,Genere)/length(Prezzo_acquisto_cat)


#### Bar charts ---------------------------------------------------------------
? barplot
#! barplot need a table
# barplot(Area_geografica)

barplot(table(Area_geografica), legend = T)
barplot(table(Area_geografica)/length(Area_geografica), legend = T)

# distribuzione doppia, distribuzioni condizionate
barplot(table(Area_geografica, Genere), legend = T)
barplot(table(Area_geografica, Genere)/length(Area_geografica), legend = T)

barplot(table(Genere,Area_geografica), legend = T)

# Usempio di utilizzo su variabili quantitative
barplot(Spesa_media_mensile[1:5] ~  Nome[1:5], legend = T)


#### Dot charts ---------------------------------------------------------------
? dotchart
dotchart(Spesa_media_mensile) # non ha un significato specifico

table(Area_geografica,Livello_istruzione)
dotchart(table(Area_geografica,Livello_istruzione))

dotchart(table(Livello_istruzione, Area_geografica))

# creiamo una tabella
tab<-matrix(0, 
            length(unique(Area_geografica)), 
            length(unique(Livello_istruzione)))

for(i in 1:length(unique(Area_geografica)))
  for(j in 1:length(unique(Livello_istruzione)))
    tab[i,j] <- mean(Spesa_media_mensile
                     [Area_geografica == unique(Area_geografica)[i] & 
                      Livello_istruzione == unique(Livello_istruzione)[j]])
tab # tabella delle medie

rownames(tab)<-levels(Area_geografica)
colnames(tab)<-levels(Livello_istruzione)
tab

dotchart(tab, xlim = c(65,90), bg = "skyblue",
          main = "Spesa media mensile per livello di istruzione", 
          xlab = "Spesa media mensile",
          ylab = "Raggruppamento:  Livello di istruzione  x Area Geografica")

# Stessa tabella pero con le aree geografiche sulle righe, 
# usiamo la funzione t()
dotchart(t(tab), xlim = c(65,90), bg = "skyblue",
          main = "Spesa media mensile per area geografica", 
          xlab = "Spesa media mensile",
          ylab = "Raggruppamento: Area Geografica x Livello di istruzione")


# Save and detach @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
detach(AcquistiOnLine)
#! Set the proper working directory
#save.image("Wks_Univariata.RData")  

# Save the data frame with the new factor
#save(AcquistiOnLine, file = "AcquistiONLine.Rda")
