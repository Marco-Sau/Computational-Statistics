# ====================== BIVARIATE ANALYSIS ===================================
rm(list = ls())
# Load data 
load("AcquistiOnLine.Rda")
summary(AcquistiOnLine)

# Attach the dataset to the environment
attach(AcquistiOnLine)

## 3.3 Paired data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Correlation ***************************************************************
#### Pearson correlation coefficient ------------------------------------------
? cor

# Correlazione campionaria
cor(Spesa_media_mensile,Numero_pagine_visitate)


#### Spearman correlation coefficient -----------------------------------------
# Sperman uses the ranks of the data
rank(Spesa_media_mensile)
Spesa_media_mensile

rank(Numero_pagine_visitate)

cor(Spesa_media_mensile,Numero_pagine_visitate, method = "pearson")
cor(Spesa_media_mensile,Numero_pagine_visitate, method = "spearman")




### Trends ********************************************************************
# Causation
#### Simple regression: lm function -------------------------------------------
? lm
lm(Spesa_media_mensile ~ Numero_pagine_visitate)
# Coefficients:
#            (Intercept)  Numero_pagine_visitate  
#                 71.898                   1.478  
# Explanation:
# 71.898 e il valore della spesa media mensile quando 
# il numero di pagine visitate e 0
# 1.478 e il valore della spesa media mensile quando 
# il numero di pagine visitate aumenta di 1

#plot(Spesa_media_mensile, Numero_pagine_visitate)

plot(Numero_pagine_visitate, Spesa_media_mensile)

as.numeric(lm(Spesa_media_mensile ~ Numero_pagine_visitate)$coefficients)
abline(a= as.numeric(lm(Spesa_media_mensile ~ 
                         Numero_pagine_visitate)$coefficients)[1],
       b= as.numeric(lm(Spesa_media_mensile ~ 
                         Numero_pagine_visitate)$coefficients)[2])

residui <- Spesa_media_mensile - lm(Spesa_media_mensile ~ 
                                  Numero_pagine_visitate)$fitted
residui

hist(residui)
hist(lm(Spesa_media_mensile ~ Numero_pagine_visitate)$residuals)

#### Regression line ----------------------------------------------------------
# Relazione causale

lm(Spesa_media_mensile ~ Prezzo_acquisto)

plot(Prezzo_acquisto, Spesa_media_mensile)

as.numeric(lm(Spesa_media_mensile ~ Prezzo_acquisto)$coefficients)
abline(a=as.numeric(lm(Spesa_media_mensile ~ Prezzo_acquisto)$coefficients)[1],
       b=as.numeric(lm(Spesa_media_mensile ~ Prezzo_acquisto)$coefficients)[2])

# fitted returns the fitted values
res<-Spesa_media_mensile - lm(Spesa_media_mensile ~ Prezzo_acquisto)$fitted
res

hist(res)

# e se escludessimo i casi Prezzo_acquisto == 0?
x<-Prezzo_acquisto[Prezzo_acquisto>0]
y<-Spesa_media_mensile[Prezzo_acquisto>0]

plot(x,y) 

lm(y ~ x) # regression line

abline(a=as.numeric(lm(y ~ x)$coefficients)[1],
       b=as.numeric(lm(y ~ x)$coefficients)[2])

res <- y - lm(y ~ x)$fitted
res
lm(y ~ x)$residuals # same result as above

hist(res) # -30 <--> 30


# An example where the fit improves a lot 
# Install the package datarium
# install.packages("datarium")

# data is used to upload the dataset
data("marketing", package = "datarium")
head(marketing, 4)

# il dataset contiene l'impatto di tre canali pubblicitari (YouTube, 
# Facebook e giornali) sulle vendite. I dati riguardano il budget 
# pubblicitario in migliaia di dollari insieme alle vendite. 
# L'esperimento pubblicitario è stato ripetuto 200 volte con budget 
# diversi

#! For our purpose we will use only youtube and sales
x<-marketing$youtube
y<-marketing$sales

plot(x,y)

lm(y ~ x)
# each 1000 dollars spent on youtube, sales increase by 0.0475 units

abline(a = as.numeric(lm(y ~ x)$coefficients)[1],
       b = as.numeric(lm(y ~ x)$coefficients)[2],
       col = 2)
# Exists a casuality relationship between youtube and sales
# Even if the results are not so good

res<-y - lm(y ~ x)$fitted
res

hist(res)

# Try to watch what happen if we add a new point to the dataset
# At first we add a point with a very high value of sales
# But the regression line doesn't change so much
# So we add a sequence of points with a very high value of sales
# The value that change significantly the regression line is
#   called leverage point (punto influente)
x3<-c(x,rep(50,50)) # At first was x2 then we change it in x3
y3<-c(y,rep(50,50))

plot(x3,y3)

lm(y3 ~ x3)

abline(a=as.numeric(lm(y3 ~ x3)$coefficients)[1],
       b=as.numeric(lm(y3 ~ x3)$coefficients)[2], col=2)

res<-y - lm(y ~ x)$fitted
res

hist(res)
  
# To mitigate the effect of leverage points we can more robust regression
# and instead of using the method of least squares we can use the method
# of median absolute deviations (MAD)
# Regression on MAD --> |y - y_stim | -> quantile regression


#### Quantile regression ------------------------------------------------------
# For some reason we could be interested on estimating any quantile
require(quantreg)
# f is the formula
f <- y ~ x # y depends on x
res_lm <- lm(f)

? rq # represent the quantile regression fit
res_rq <- rq(f, tau=0.5) # tau is the quantile we want to estimate
res_rq
plot(f)
abline(res_lm, lty=1)
abline(res_rq, lty=2, col=2)
legend(0, 30, 
       c("lm -> least squared method", "rq -> median absolute method"), 
       lty=1:2, 
       col=1:2)
 
# Let's check the difference in an unbalanced dataset which has been 
# created above
f3 <- y3 ~ x3 # f is the formula
res_lm3 <- lm(f3)
res_rq3 <- rq(f3, tau=0.5)
plot(f3)
abline(res_lm3, lty=1)
abline(res_rq3, lty=2, col=2)
legend(0, 35, 
       c("lm -> least squared method", "rq -> median absolute method"), 
       lty=1:2, 
       col=1:2)

# The advantage of the median absolute method is that it is more robust
# Infact from the graph is clear that the regression line is not affected
# by the leverage points

# Now we can try to estimate the quantile regression for different quantiles
res_rq3q1 <- rq(f3, tau=0.25) # first quartile
res_rq3q3 <- rq(f3, tau=0.75)  # third quartile

abline(res_rq3q1, lty=2, col=3)
abline(res_rq3q3, lty=2, col=4)
legend(150, 50, c(
                  "rq -> quartile 0.25", 
                  "rq -> quartile 0.75"), 
      lty=2, col=3:4)




### Trasformations ************************************************************
# trasformazioni di variabili   
# x is youtube and y is sales
# Our assumptions are that x and y are normally distributed and that
#   the variance of y is constant
# The main transformations are:
#   - log
#   - square root

#### log transformation  ------------------------------------------------------
# First we check if the distribution follows a normal distribution (Gaussian)
plot(x,y)

# the more the points are close to the line the more the distribution is normal
qqnorm(x) # quantile quantile plot of x
qqnorm(y) # quantile quantile plot of y

# make another check with the density plot
plot(density(x))
plot(density(y))

# Let's try to apply a log transformation
# The variance is reduced
plot(density(log(x)))
plot(density(log(y)))

# Make a plot with the log transformation
plot(log(x),log(y))
x2 <- log(x)
y2 <- log(y)
f2 <- y2 ~ x2
res_lm2 <- lm(f2) # estimated model
res_lm2
res_lm2$coefficients
res_lm2$residuals
# y stimate

# Compare the residuals of the two models, 
# with and without the log transformation
par(mfrow=c(2,1))
hist(res_lm$residuals)
hist(res_lm2$residuals)

# Let's check the standard deviation of the residuals
sd(res_lm$residuals)
sd(res_lm2$residuals)

# Standard deviation line  


### Alternative trends line ***************************************************
#### Robust regression --------------------------------------------------------
# It's based on the M estimators
# The M estimators attribute a weight to each observation, and the weight
# depends on the distance between the observation and the regression line
# If the distance is small the least square method is used, otherwise 
# the robust regression is used

library(MASS)
# normal regression
res_lm1  <-  lm(f)
res_rlm <- rlm(f)
par(mfrow=c(1,1))
plot(f)
abline(res_lm1,  lty=1)
abline(res_rlm, lty=2, col=2)
legend(10, 40, legend=c("lm1", "rlm"), lty=1:2, col=1:2)

# with outliers
res_lm3  <-  lm(f3)
res_rlm <- rlm(f3)
par(mfrow=c(1,1))
plot(f3)
abline(res_lm3,  lty=1)
abline(res_rlm, lty=2, col=2)
legend(10, 40, legend=c("lm3", "rlm"), lty=1:2, col=1:2)
             
      
#### Smoothers Loess ----------------------------------------------------------
# semi-parametric regression
# local regression
# Smoothers Estimators (Loess) 
# span = 3/4 -> 3/4 of the data
plot(f, col=rgb(0,0,0, alpha=0.5))
res_lm <- lm(f)
abline(res_lm, lty=1, lwd=2)
##
? loess
# We have attached the dataset so we don't need to pass the data
# argument to the function

res_loess<-loess(f,  degree=1)  # degree is the degree of the polynomial
lines(loess.smooth(x, y,  degree=1), lty=2, lwd=2, col=2)
legend(10, 30, legend=c("lm", "loess"), lty=1:2, col=1:2)

# use different span
plot(f, col=rgb(0,0,0, alpha=0.5))
abline(res_lm, lty=1, lwd=2)
lines(loess.smooth(x, y,  degree=1), lty=2, lwd=2, col=2)
lines(loess.smooth(x, y, span=1/10,  degree=1), lty=1, lwd=2, col=3)
legend(10, 30, legend=c("lm", "loess(3/4)", "loess(1/10"), 
       lty=c(1,2,1), col=1:3)

# use different degree
plot(f, col=rgb(0,0,0, alpha=0.5))
abline(res_lm, lty=1, lwd=2)
lines(loess.smooth(x, y,  degree=1), lty=2, lwd=2, col=2)
lines(loess.smooth(x, y, span=1/10,  degree=1), lty=1, lwd=2, col=3)
lines(loess.smooth(x, y, span=1/10,  degree=2), lty=2, lwd=2, col=4)
legend(5, 33, legend=c("lm", "loess(3/4),1", "loess(1/10),1",
                        "loess(1/10),2"), 
       lty=c(1,2,1,2), col=1:4)
  
    
## 3.4 Bivariate categorial data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Two-way tables ----------------------------------------------------------
# Qualitative variables
table(Area_geografica, Livello_istruzione)


#### Marginal distributions of two-way tables ---------------------------------
# Totale per riga
margin.table(table(Area_geografica, Livello_istruzione), margin = 1)
# Totale per colonna
margin.table(table(Area_geografica, Livello_istruzione), margin = 2)

# Tabella completa
addmargins(table(Area_geografica, Livello_istruzione))
# To get the relative frequencies divide by the total
addmargins(table(Area_geografica, Livello_istruzione)/length(Area_geografica))


##### Conditional distributions of two-way tables -----------------------------
prop.table(table(Area_geografica, Livello_istruzione)) # Frequenze relative
# Distribuzioni condizionate per riga
prop.table(table(Area_geografica, Livello_istruzione), margin = 1) # x | y
# Distribuzioni condizionate per colonna
prop.table(table(Area_geografica, Livello_istruzione), margin = 2) # y | x


#### xtabs function -----------------------------------------------------------
# uses a formula object
# calculate the frequencies

# same as table but with a formula
xtabs( ~ Area_geografica + Livello_istruzione) 

# which is the total number of visited pages 
# for each area and level of education?
xtabs(Numero_pagine_visitate ~ Area_geografica + Livello_istruzione)

# which is the average bill for each area and level of education?
xtabs(Spesa_media_mensile ~ Area_geografica + Livello_istruzione)


#### ftable function ----------------------------------------------------------
# Particolari distribuzioni condizionate
# ftable is a function that allows to create a table with more 
#   than two variables
# it's like we are considering a double distribution conditioned 
#   on a third variable
# x is the variable Area geografica, y is Livello istruzione and z is Genere
tab <- xtabs( ~ Area_geografica + Livello_istruzione + Genere)
tab

# The value are all conditional
# sulle righe abbiamo il livello di istruzione che si trova al secondo posto
# nella formula di xtabs
# nelle colonne abbiamo prima l'area geografica e poi il genere
ftable(tab, row.vars = 2, col.vars = c(1,3))

# in termini di proporzioni
prop.table(tab)
prop.table(ftable(tab, row.vars = 2, col.vars = c(1,3)))


### Graphical representations of conditional distributions ********************
#### Bar plot ------------------------------------------------------------------
# Overlapping barplot
# diagrammi a barre sovrapposte
barplot(table(Area_geografica,Livello_istruzione))

# Side by side barplot
barplot(table(Area_geografica,Livello_istruzione),beside = T)
barplot(table(Livello_istruzione, Area_geografica),beside = T)

#### Mosaic plot ---------------------------------------------------------------
# Takes in input a table
# single variable
mosaicplot(table(Genere))

# double variable
mosaicplot(table(Genere, Acquisto_online))

# triple variable
tab<-xtabs(Numero_pagine_visitate ~ 
             Area_geografica + Livello_istruzione)
mosaicplot(tab)

# quadruple variable
tab<-xtabs(Numero_pagine_visitate ~ 
             Area_geografica + Livello_istruzione + Genere)
mosaicplot(tab)


### Measure of association for categorical data *******************************
# Correlation between ordinal variables 
#### Kendall's correlation coefficient ----------------------------------------
# Take into account the order of the values and also 
# the concordance and discordance
# Recap:
# - Pearson's correlation coefficient: 
#     takes into account only the order
# - Spearman's correlation coefficient: 
#     takes into account the rank
# - Kendall's correlation coefficient: 
#     takes into account the order and the concordance/discordance
summary(AcquistiOnLine)
table(Livello_istruzione, Prezzo_acquisto_cat)

# To use the function cor() we need to convert the variables into numeric
cor(as.numeric(Livello_istruzione),as.numeric(Prezzo_acquisto_cat), 
    method = "kendall")
# The result is 0.07756. How do we interpret it?
# Let's image that we had supposed that the student with a master degree
#   would have bought more expensive books than 
#   the student with a bachelor degree.
# The correlation coefficient is almost zero, se we were wrong.

# The Kendall test is explained here:
# http://www.cma4ch.org/chemo/heritage/correlazione/rslide33.html


# Kendall tau test
# Factorial function ----------------------------------------------------------
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

#? test
#factorial(5) # 120


# Binomial coefficient function -----------------------------------------------
binomial_coefficient <- function(n, k) {
  if (k < 0 || k > n) {
    stop("Invalid values for n and k. k must be between 0 and n.")
  }
  result <- factorial(n) / (factorial(k) * factorial(n - k))
  return(result)
}

#? test
#n_value <- 5
#k_value <- 2
#result <- binomial_coefficient(n_value, k_value) # 10
#cat("C(", n_value, ",", k_value, ") =", result, "\n")


# Kendall's Tau a -------------------------------------------------------------
# Don't take into account ties
kendall_tau_a <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Input vectors must have the same length.")
  }
  
  n <- length(x)
  sum_terms <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sum_terms <- sum_terms + sign(x[i] - x[j]) * sign(y[i] - y[j])
    }
  }
  
  tau_a <- sum_terms / binomial_coefficient(n, 2)
  
  return(tau_a)
}


# Kendall's Tau b -------------------------------------------------------------
# Take into account ties
kendall_tau_b <- function(x, y) {
  if (length(x) != length(y)) {
    stop("Input vectors must have the same length.")
  }
  
  n <- length(x)
  sum_terms <- 0
  tie_x <- 0
  tie_y <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      sum_terms <- sum_terms + sign(x[i] - x[j]) * sign(y[i] - y[j])
      # Check for ties
      if (x[i] == x[j]) {
        tie_x <- tie_x + 1
      }
      if (y[i] == y[j]) {
        tie_y <- tie_y + 1
      }
    }
  }
  
  tau_b <- sum_terms / 
    sqrt((binomial_coefficient(n, 2) - tie_x) * 
           (binomial_coefficient(n, 2) - tie_y)) # choose is a coefficient binomial
  
  return(tau_b)
}

# Kendall tau without ties ----------------------------------------------------
kendall_tau_no_ties <- function(x, y) {
  n <- length(x)
  concordant_pairs <- 0
  discordant_pairs <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Count concordant and discordant pairs
      concordant_pairs <- concordant_pairs + (x[i] - x[j]) * (y[i] - y[j]) > 0
      discordant_pairs <- discordant_pairs + (x[i] - x[j]) * (y[i] - y[j]) < 0
    }
  }
  
  # Calculate Kendall tau without ties
  tau_no_ties <- (concordant_pairs - discordant_pairs) / 
    sqrt((concordant_pairs + discordant_pairs) * (n * (n - 1)) / 2)
  
  return(tau_no_ties)
}


# Kendall tau with ties -------------------------------------------------------
kendall_tau_with_ties <- function(x, y) {
  n <- length(x)
  concordant_pairs <- 0
  discordant_pairs <- 0
  tied_pairs <- 0
  
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      # Count concordant, discordant, and tied pairs
      concordant_pairs <- concordant_pairs + (x[i] - x[j]) * (y[i] - y[j]) > 0
      discordant_pairs <- discordant_pairs + (x[i] - x[j]) * (y[i] - y[j]) < 0
      tied_pairs <- tied_pairs + (x[i] == x[j]) | (y[i] == y[j])
    }
  }
  
  # Calculate Kendall tau with ties
  tau_with_ties <- (concordant_pairs - discordant_pairs) / 
    sqrt((concordant_pairs + discordant_pairs + tied_pairs) * (n * (n - 1)) / 2)
  
  return(tau_with_ties)
}


# test with our data ----------------------------------------------------------
# Create a data frame with 3 columns and 7 rows
students_grades <- data.frame(
  students = c("an", "fr", "lu", "pi", "ri", "si", "st"),
  grades_1 = c(27, 23, 27, 30, 28, 24, 25),
  grades_2 = c(27, 24, 28, 29, 30, 23, 25)
)
# Save the data frame as a .csv file
#write.csv(students_grades, "12/students_grades.csv", row.names = FALSE)

# Read the data frame from the .csv file
#students_grades <- 
# read.csv("12/students_grades.csv" , header = TRUE, sep = ",")
# Print the data frame
print(students_grades)
summary(students_grades)

attach(students_grades)

# Calculate Kendall tau a and b with our functions
# With or without the rank function, the results are the same
rank_grades_1 <- rank(grades_1)
rank_grades_2 <- rank(grades_2)

cat("Kendall tau a:", kendall_tau_a(rank_grades_1, rank_grades_2), "\n")
cat("Kendall tau b:", kendall_tau_b(rank_grades_1, rank_grades_2), "\n")
#cat("Kendall tau without ties:", kendall_tau_no_ties(grades_1, grades_2), "\n")
#cat("Kendall tau with ties:", kendall_tau_with_ties(grades_1, grades_2), "\n")

# Calculate Kendall tau with the cor function
# The cor function takes into account ties
cat("Kendall tau with 'cor' function:", 
    cor(grades_1, grades_2, method = "kendall"), "\n")

# Apply the Kendall tau function to the data frame: AcquistiOnLine ------------
# Load the data
load("AcquistiOnLine.Rda")
summary(AcquistiOnLine)
attach(AcquistiOnLine)

# Calculate Kendall tau a and b with our functions
cat("Kendall tau a", 
    kendall_tau_a(as.numeric(Livello_istruzione),
                  as.numeric(Prezzo_acquisto_cat)))
cat("Kendall tau b", 
    kendall_tau_b(as.numeric(Livello_istruzione),
                  as.numeric(Prezzo_acquisto_cat)))

# Calculate Kendall tau with the cor function
cat("Kendall tau with 'cor' function:", 
    cor(as.numeric(Livello_istruzione),
        as.numeric(Prezzo_acquisto_cat), 
        method = "kendall"), "\n")

# Detach the data frames
#detach(AcquistiOnLine)
#detach(students_grades)



#### Chi-square ---------------------------------------------------------------
# Compare the observed frequencies with the expected frequencies
# If chi-square == 0 then the variables are independent
tbl<-xtabs(~Livello_istruzione+Area_geografica)           
summary(tbl)
 
chisq.test(tbl)$expected # expected frequencies (n^ in the formula)
fo <- tbl # observed frequencies (n in the formula)
fe <- chisq.test(tbl)$expected # expected frequencies (n^ in the formula)
(fo - fe)^2 / fe
sum((fo - fe)^2 / fe)


#### Deviance -----------------------------------------------------------------
# If deviance == 0 then the variables are independent
fo <- tbl
fe <- chisq.test(tbl)$expected
(fo - fe)^2 / fe
sum((fo - fe)^2 / fe) # chi-squared
2 * sum(fo * log(fo/fe)) # deviance


#### Odds ratio in a 2 x 2 table ----------------------------------------------
# The probability of success is the probability of the event 
#   that we are interested in
# respect to the probability of the complementary event
table(Genere, Acquisto_online)

# Calcoliamo le probabilita' che un cliente sia femmina
odd <- function(x) x[1,1] * x[2,2] / (x[1,2] * x[2,1])

odd(table(Genere, Acquisto_online))
