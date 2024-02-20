# ===== VARIABILI CASUALI DISCRETE E CONTINUE (TUTTE) =========================
####################         DISCRETE                     #####################
# ******************           Uniforme                   **********************
# ?dunif
# Function for Probability Mass Function (PMF) - Uniform
Fprob_Unif <- function(x, min, max, plot=TRUE){
  max <- max + 1
  ris <- dunif(x, min, max)
  if(plot == TRUE){
    par(bg="white")
    plot(min:(max-1), 
         ris, 
         type="h", 
         xlab="Realizations",
         ylab="P. Mass Function", 
         main = "Uniform Distribution\nProbability Mass Function")
    points(min:(max-1), ris, col=2, pch=16, cex =1.5)}
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Uniform
Frip_Unif <- function(x, min, max, plot=TRUE){
  ris <- punif(x, min, max)
  if(plot == TRUE){
    par(bg="white")
    plot(min:max, 
         ris, 
         type="s", 
         xlab="Realizations",
         ylab="P. Mass Function", 
         main = "Uniform Distribution\nCumulative Distribution Function")
    points(min:max, ris, col=2, pch=16, cex =1.5)}
  return(ris)
}

# Function for Quantiles - Uniform
Q_unif <- function(min, max){
  ris <- round(qunif(c(.01, .25, .5, .75, .99), min, max),0)
  return(ris)
}

# Function for generating random variates - Uniform
R_unif <- function(n, min, max){
  ris <- round(runif(n, min, max),0)
  return(ris)
}

# Main function for Uniform Distribution
VC_UniformeDiscreta <- function(x, n, min, max, plot=TRUE){
  fprob <- Fprob_Unif(x, min, max, plot=plot)
  frip  <- Frip_Unif(x, min, max, plot=plot)
  nris <- (max - min) + 1
  va <- (nris+1)/2
  var <- (nris^2-1)/12
  q <- Q_unif(min, max)
  r <- R_unif(n, min, max)
  
  # Print results
  cat("The probability mass function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris<-list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Uniform Distribution function in 1:6
xx<-VC_UniformeDiscreta(1:6, 50, 1, 6, plot = TRUE)




# ******************           Bernoulli                  ********************** 
# ?dbinom
# Function for Probability Mass Function (PMF) - Bernoulli
Fprob_Bernoulli <- function(x, prob, plot = TRUE) {
  ris <- dbinom(x, 1, prob)  # Bernoulli distribution with n=1
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "h", 
         xlab = "Realizations",
         ylab = "P. Mass Function", 
         main = "Bernoulli Distribution\nProbability Mass Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Bernoulli
Frip_Bernoulli <- function(x, prob, plot = TRUE) {
  ris <- pbinom(x, 1, prob)  # Bernoulli distribution with n=1
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "s", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Bernoulli Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Bernoulli
Q_Bernoulli <- function(prob) {
  ris <- qbinom(c(0.01, 0.25, 0.5, 0.75, 0.99), 1, prob)  
  # Bernoulli distribution with n=1
  return(ris)
}

# Function for generating random variates - Bernoulli
R_Bernoulli <- function(n, prob) {
  ris <- rbinom(n, 1, prob)  # Bernoulli distribution with n=1
  return(ris)
}

# Main function for Bernoulli Distribution
VC_Bernoulli <- function(x, n, prob, plot = TRUE) {
  fprob <- Fprob_Bernoulli(x, prob, plot = plot)
  frip <- Frip_Bernoulli(x, prob, plot = plot)
  va <- prob
  var <- prob * (1 - prob)
  q <- Q_Bernoulli(prob)
  r <- R_Bernoulli(n, prob)
  
  # Print results
  cat("The probability mass function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Bernoulli Distribution function with p = 0.35 and range 1:6
xx <- VC_Bernoulli(1:6, 50, 0.35, plot = TRUE)




# ******************           Binomiale                  **********************
# ?dbinom
# Function for Probability
# x is a vector of quantiles, size is the number of trials, 
# prob is the probability of success
Fprob_Binom <- function(x, size, prob, plot = TRUE) {
  ris <- dbinom(x, size, prob) 
  if (plot == TRUE) {
    x_vals <- 0:size
    par(bg = "white")
    plot(x_vals, 
         ris, 
         type = "h", 
         xlab = "Realizations",
         ylab = "P. Mass Function", 
         main = "Binomial Distribution\nProbability Mass Function")
    points(x_vals, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF)
Frip_Binom <- function(x, size, prob, plot = TRUE) {
  ris <- pbinom(x, size, prob)
  if (plot == TRUE) {
    x_vals <- 0:size  # Adjusted x_vals to match the length of 'x'
    par(bg = "white")
    plot(x_vals, 
         ris, 
         type = "s", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Binomial Distribution\nCumulative Distribution Function")
    points(x_vals, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles
Q_Binom <- function(size, prob) {
  ris <- qbinom(c(0.01, 0.25, 0.5, 0.75, 0.99), size, prob)
  return(ris)
}

# Function for generating random variates
R_Binom <- function(n, size, prob) {
  ris <- rbinom(n, size, prob)
  return(ris)
}

# Main function for Binomial Distribution
VC_Binomial <- function(x, n, size, prob, plot = TRUE) {
  fprob <- Fprob_Binom(x, size, prob, plot = plot)
  frip <- Frip_Binom(x, size, prob, plot = plot)
  va <- size * prob
  var <- size * prob * (1 - prob)
  q <- Q_Binom(size, prob)
  r <- R_Binom(n, size, prob)
  
  # Print results
  cat("The probability mass function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Binomial Distribution function with p = 0.35 and n = 10
xx <- VC_Binomial(0:10, 50, 10, 0.35, plot = TRUE)
# 0:10 is the vector of quantiles
# 50 is the number of random variates to generate
# 10 is the number of trials
# 0.35 is the probability of success




# ******************           Ipergeometrica                  *****************
#? dhyper
# Function for Probability Mass Function (PMF) - Hypergeometric
# x = n, m = M, n = N - m
Fprob_Hypergeo <- function(x, m, n, N, plot = TRUE) {
  ris <- dhyper(x, m, N - m, n)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "h", 
         xlab = "Realizations",
         ylab = "Probability Function", 
         main = "Hypergeometric Distribution\nProbability Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Hypergeometric
Frip_Hypergeo <- function(x, m, n, N, plot = TRUE) {
  ris <- phyper(x, m, N - m, n)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "s", 
         xlab = "Realizations",
         ylab = "Cumulative Distribution", 
         main = "Hypergeometric Distribution\nCumulative Distribution")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Hypergeometric
Q_Hypergeo <- function(m, n, N) {
  ris <- qhyper(c(0.01, 0.25, 0.5, 0.75, 0.99), m, N - m, n)
  return(ris)
}

# Function for generating random variates - Hypergeometric
R_Hypergeo <- function(n, m, N, nsim = 1) {
  ris <- rhyper(nsim, m, N - m, n)
  return(ris)
}

# Main function for Hypergeometric Distribution
VC_Hypergeo <- function(x, n, m, N, plot = TRUE) {
  fprob <- Fprob_Hypergeo(x, m, n, N, plot = plot)
  frip <- Frip_Hypergeo(x, m, n, N, plot = plot)
  va  <- n * m / N
  var <- (n * m * (N - m) * (N - n)) / (N^2 * (N - 1))
  q <- Q_Hypergeo(m, n, N)
  r <- R_Hypergeo(n, m, N, nsim = 50)
  
  # Print results
  cat("The probability function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Hypergeometric Distribution function with N = 100, M = 30, and n = 20
xx <- VC_Hypergeo(0:20, 50, 30, 100, plot = TRUE)
# 1:20 is the vector of quantiles
# 50 is the number of random variates to generate
# 30 is the number of successes in the population
# 100 is the population size




# ******************           Poisson                  **********************
#?dpois
# Function for Probability Mass Function (PMF) - Poisson
Fprob_Poisson <- function(x, lambda, plot = TRUE) {
  ris <- dpois(x, lambda)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "h", 
         xlab = "Realizations",
         ylab = "P. Mass Function", 
         main = "Poisson Distribution\nProbability Mass Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Poisson
Frip_Poisson <- function(x, lambda, plot = TRUE) {
  ris <- ppois(x, lambda)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "s", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Poisson Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Poisson
Q_Poisson <- function(lambda) {
  ris <- qpois(c(0.01, 0.25, 0.5, 0.75, 0.99), lambda)
  return(ris)
}

# Function for generating random variates - Poisson
R_Poisson <- function(n, lambda) {
  ris <- rpois(n, lambda)
  return(ris)
}

# Main function for Poisson Distribution
VC_Poisson <- function(x, n, lambda, plot = TRUE) {
  fprob <- Fprob_Poisson(x, lambda, plot = plot)
  frip <- Frip_Poisson(x, lambda, plot = plot)
  va <-  lambda
  var <- lambda
  q <- Q_Poisson(lambda)
  r <- R_Poisson(n, lambda)
  
  # Print results
  cat("The probability mass function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Poisson Distribution function with lambda = 3 and range 0:10
xx <- VC_Poisson(0:10, 50, 3, plot = TRUE)




# ******************           Geometrica                  ********************
#> ?dgeom
# Function for Probability Mass Function (PMF) - Geometric
Fprob_Geometric <- function(x, prob, plot = TRUE) {
  ris <- dgeom(x, prob)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "h", 
         xlab = "Realizations",
         ylab = "P. Mass Function", 
         main = "Geometric Distribution\nProbability Mass Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Geometric
Frip_Geometric <- function(x, prob, plot = TRUE) {
  ris <- pgeom(x, prob)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "s", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Geometric Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Geometric
Q_Geometric <- function(prob) {
  ris <- qgeom(c(0.01, 0.25, 0.5, 0.75, 0.99), prob)
  return(ris)
}

# Function for generating random variates - Geometric
R_Geometric <- function(n, prob) {
  ris <- rgeom(n, prob)
  return(ris)
}

# Main function for Geometric Distribution
VC_Geometric <- function(x, n, prob, plot = TRUE) {
  fprob <- Fprob_Geometric(x, prob, plot = plot)
  frip <- Frip_Geometric(x, prob, plot = plot)
  va <-  1 / prob
  var <- (1 - prob) / prob^2
  q <- Q_Geometric(prob)
  r <- R_Geometric(n, prob)
  
  # Print results
  cat("The probability mass function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Call the Geometric Distribution function with p = 0.2 and range 0:20
xx <- VC_Geometric(0:20, 50, 0.2, plot = TRUE)








####################         CONTINUE                     #####################
# ******************           Uniforme                   **********************
#? dunif
# Function for Probability Density Function (PDF) - Uniform Continuous
Fprob_UniformCont <- function(x, min_val, max_val, plot = TRUE) {
  ris <- dunif(x, min_val, max_val)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "P. Density Function", 
         main = "Uniform Continuous Distribution\nProbability Density Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Uniform Continuous
Frip_UniformCont <- function(x, min_val, max_val, plot = TRUE) {
  ris <- punif(x, min_val, max_val)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Uniform Continuous Distribution
         \nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Uniform Continuous
Q_UniformCont <- function(min_val, max_val) {
  ris <- c(min_val, max_val, mean(c(min_val, max_val)))
  return(ris)
}

# Function for generating random variates - Uniform Continuous
R_UniformCont <- function(n, min_val, max_val) {
  ris <- runif(n, min_val, max_val)
  return(ris)
}

# Main function for Uniform Continuous Distribution
VC_UniformCont <- function(x, n, min_val, max_val, plot = TRUE) {
  fprob <- Fprob_UniformCont(x, min_val, max_val, plot = plot)
  frip <- Frip_UniformCont(x, min_val, max_val, plot = plot)
  va <- (min_val + max_val) / 2
  var <- (max_val - min_val)^2 / 12
  q <- Q_UniformCont(min_val, max_val)
  r <- R_UniformCont(n, min_val, max_val)
  
  # Print results
  cat("The probability density function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Define range and parameters, then call the 
# Uniform Continuous Distribution function
range_vals <- seq(2, 8, length.out = 1000)  
# Adjust the length.out for smoother plots
xx <- VC_UniformCont(range_vals, 50, 2, 8, plot = TRUE)




# ******************           Esponenziale               **********************
#? dexp
# Function for Probability Density Function (PDF) - Exponential
Fprob_Exponential <- function(x, rate, plot = TRUE) {
  ris <- dexp(x, rate)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "P. Density Function", 
         main = "Exponential Distribution\nProbability Density Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Exponential
Frip_Exponential <- function(x, rate, plot = TRUE) {
  ris <- pexp(x, rate)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Exponential Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Exponential
Q_Exponential <- function(rate) {
  ris <- qexp(c(0.01, 0.25, 0.5, 0.75, 0.99), rate)
  return(ris)
}

# Function for generating random variates - Exponential
R_Exponential <- function(n, rate) {
  ris <- rexp(n, rate)
  return(ris)
}

# Main function for Exponential Distribution
VC_Exponential <- function(x, n, rate, plot = TRUE) {
  fprob <- Fprob_Exponential(x, rate, plot = plot)
  frip <- Frip_Exponential(x, rate, plot = plot)
  va <- VA_exponential <- 1 / rate
  var <- VAR_exponential <- 1 / rate^2
  q <- Q_Exponential(rate)
  r <- R_Exponential(n, rate)
  
  # Print results
  cat("The probability density function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Define range and parameters, then call the Exponential Distribution function
range_vals <- seq(0, 10, length.out = 1000)  
# Adjust the length.out for smoother plots
xx <- VC_Exponential(range_vals, 50, 0.2, plot = TRUE)




# ******************           Normale                   **********************
dnorm(2, 5, sqrt(4))
# ? dnorm

media<-5
sd<-sqrt(4)

Fprob_Norm <- function(x, media, sd, plot=TRUE){
  if(is.null(x)) x <- seq(qnorm(.01, media, sd),
                          qnorm(.99, media, sd),
                          length.out = 100)
  ris<- dnorm(x, media, sd)
  if(plot == TRUE){
    par(bg="white")
    plot(x, 
         ris, 
         type="h", 
         xlab="Realizzazioni",
         ylab="F. di densità di probabilità", 
         main = "V.C. Normale\nFunzione di Probabilità")
    points(x, 
           ris, 
           col=2, 
           pch=16, 
           cex =1.5)}
  return(ris)
}

Frip_Norm <- function(x, media, sd, plot=TRUE){
  if(is.null(x)) x <- seq(qnorm(.01, media, sd),
                          qnorm(.99, media, sd),
                          length.out = 100)
  ris<- pnorm(x, media, sd)
  if(plot == TRUE){
    par(bg="white")
    plot(x, 
         ris, 
         type="s", 
         xlab="Realizzazioni",
         ylab="F. di Ripartizione", 
         main = "V.C. Normale\nFunzione di Ripartizione")
    points(x, 
           ris, 
           col=2, 
           pch=16, 
           cex =1.5)}
  return(ris)
}



Q_Norm<-function(media, sd){
  ris <- qnorm(c(.01, .25, .5, .75, .99), media, sd)
  return(ris)
}

R_Norm<-function(n, media, sd){
  ris <- rnorm(n, media, sd)
  return(ris)
}

VC_Normale<-function(x=NULL, n, media, sd, plot=TRUE){
  fprob <- Fprob_Norm(x, media, sd, plot=plot)
  frip <- Frip_Norm(x, media, sd, plot=plot)
  va <- media
  var <- sd^2
  q <- Q_Norm(media, sd)
  r <- R_Norm(n, media, sd)
  # Stampa risultati
  cat("I valori della f. di prob. sono:", fprob, "\n")
  cat("I valori della f. di ripart. sono:", frip, "\n")
  cat("I principali quantili sono:", q, "\n")
  cat("Il valore atteso vale:", va, "\n")
  cat("La varianza vale", var, "\n")
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

xx <- VC_Normale(n=10, media = 50, sd = 7, plot = TRUE)
# n=10 è il numero di realizzazioni generate




# ******************           Chi-Quadro          **********************
#?dchisq
# Function for Probability Density Function (PDF) - Chi-square
Fprob_ChiSquare <- function(x, df, plot = TRUE) {
  ris <- dchisq(x, df)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "P. Density Function", 
         main = "Chi-square Distribution\nProbability Density Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Chi-square
Frip_ChiSquare <- function(x, df, plot = TRUE) {
  ris <- pchisq(x, df)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Chi-square Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Chi-square
Q_ChiSquare <- function(df) {
  ris <- qchisq(c(0.01, 0.25, 0.5, 0.75, 0.99), df)
  return(ris)
}

# Function for generating random variates - Chi-square
R_ChiSquare <- function(n, df) {
  ris <- rchisq(n, df)
  return(ris)
}

# Main function for Chi-square Distribution
VC_ChiSquare <- function(x, n, df, plot = TRUE) {
  fprob <- Fprob_ChiSquare(x, df, plot = plot)
  frip <- Frip_ChiSquare(x, df, plot = plot)
  va <- VA_chisquare <- df
  var <- VAR_chisquare <- 2 * df
  q <- Q_ChiSquare(df)
  r <- R_ChiSquare(n, df)
  
  # Print results
  cat("The probability density function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Define range and parameters, then call the Chi-square Distribution function
range_vals <- seq(0, 20, length.out = 100)  
# Adjust the length.out for smoother plots
xx <- VC_ChiSquare(range_vals, 50, 5, plot = TRUE)




# ******************           t di Student          **********************
# Function for Probability Density Function (PDF) - Student's t
Fprob_TDist <- function(x, df, plot = TRUE) {
  ris <- dt(x, df)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "P. Density Function", 
         main = "Student's t Distribution\nProbability Density Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5) # cex
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Student's t
Frip_TDist <- function(x, df, plot = TRUE) {
  ris <- pt(x, df)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Student's t Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Student's t
Q_TDist <- function(df) {
  ris <- qt(c(0.01, 0.25, 0.5, 0.75, 0.99), df)
  return(ris)
}

# Function for generating random variates - Student's t
R_TDist <- function(n, df) {
  ris <- rt(n, df)
  return(ris)
}

# Main function for Student's t Distribution
VC_TDist <- function(x, n, df, plot = TRUE) {
  fprob <- Fprob_TDist(x, df, plot = plot)
  frip <- Frip_TDist(x, df, plot = plot)
  va <- ifelse(df > 1, 0, NaN)
  var <- ifelse(df > 2, df / (df - 2), ifelse(df > 1, Inf, NaN))
  q <- Q_TDist(df)
  r <- R_TDist(n, df)
  
  # Print results
  cat("The probability density function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Define range and parameters, then call the Student's t Distribution function
range_vals <- seq(-5, 5, length.out = 100)
xx <- VC_TDist(range_vals, 50, 10, plot = TRUE)




# ******************           F di Fisher          **********************
# Function for Probability Density Function (PDF) - Fisher's F
Fprob_FDist <- function(x, df1, df2, plot = TRUE) {
  ris <- df(x, df1, df2)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "P. Density Function", 
         main = "Fisher's F Distribution\nProbability Density Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Cumulative Distribution Function (CDF) - Fisher's F
Frip_FDist <- function(x, df1, df2, plot = TRUE) {
  ris <- pf(x, df1, df2)
  if (plot == TRUE) {
    par(bg = "white")
    plot(x, 
         ris, 
         type = "l", 
         xlab = "Realizations",
         ylab = "C. Distribution Function", 
         main = "Fisher's F Distribution\nCumulative Distribution Function")
    points(x, ris, col = 2, pch = 16, cex = 1.5)
  }
  return(ris)
}

# Function for Quantiles - Fisher's F
Q_FDist <- function(df1, df2) {
  ris <- qf(c(0.01, 0.25, 0.5, 0.75, 0.99), df1, df2)
  return(ris)
}

# Function for generating random variates - Fisher's F
R_FDist <- function(n, df1, df2) {
  ris <- rf(n, df1, df2)
  return(ris)
}

# Main function for Fisher's F Distribution
VC_FDist <- function(x, n, df1, df2, plot = TRUE) {
  fprob <- Fprob_FDist(x, df1, df2, plot = plot)
  frip <- Frip_FDist(x, df1, df2, plot = plot)
  va <- ifelse(df2 > 2, df2 / (df2 - 2), ifelse(df2 > 1, Inf, NaN))
  var <- ifelse(df2 > 4, 2 * df2^2 * (df1 + df2 - 2) / 
                  (df1 * (df2 - 2)^2 * (df2 - 4)), NaN)
  q <- Q_FDist(df1, df2)
  r <- R_FDist(n, df1, df2)
  
  # Print results
  cat("The probability density function values are:", fprob, "\n")
  cat("The cumulative distribution function values are:", frip, "\n")
  cat("The main quantiles are:", q, "\n")
  cat("The expected value (mean) is:", va, "\n")
  cat("The variance is:", var, "\n")
  
  ris <- list(fprob, frip, va, var, q, r)
  return(ris)
}

# Define range and parameters, then call the Fisher's F Distribution function
range_vals <- seq(0, 5, length.out = 100) 
xx <- VC_FDist(range_vals, 50, 5, 8, plot = TRUE)





####################         COMPARAZIONI                 #####################
# ******************  Ipergeometrica -> Binomiale          ********************
#Consideriamo un'urna con 18 palline di cui 6 bianche e 12 nere.
#Estraiamo 5 palline senza reintroduzione.
#P( 2 bianche su 5 estrazioni) =?

# Bibomiale
# n = 5
# x = 2
# p = 6/18 = 1/3 = 0.3333333
ris_bin <- dbinom(2, 5, 6/18)
cat("Valore iniziale della binomiale =", ris_bin, "\n")

# Hypergeometric
ris_hyp <- dhyper(2, 6, 12, 5)
cat("Valore iniziale della ipergeometrica =", ris_hyp, "\n")

# Increase the N, the population size, and see what happens

ris_bin <- c()
ris_hyp <- c()
vector <- seq(1, 1000, 2)

for (N in vector) {
  ris_bin <- c(ris_bin, dbinom(2, 5, (6*N)/(18*N)))
  #  cat("Prob. Binom =", N, ":", ris_bin, "\n")
  ris_hyp <- c(ris_hyp, dhyper(2, 6 * N, 12 * N, 5))
  #  cat("Prob. Hyper =", N, ":", ris_hyp, "\n")
}

#ris_bin
#ris_hyp

par(bg="white")
plot(ris_bin, type = "p", col = "red", ylim = c(0.328, 0.335),
     xlim = c(0, 200), xlab = "N", ylab = "Probabilità")
points(ris_hyp, type = "p", col = "blue")
legend("topright", legend = c("Binomiale", "Ipergeometrica"),
       col = c("red", "blue"), pch = c(1, 1))



# ******************       Binomiale -> Poisson       ***********************
#Consideriamo un'urna con 18 palline di cui 6 bianche e 12 nere.
#Estraiamo 5 palline senza reintroduzione.
#P( 2 bianche su 5 estrazioni) =?

# Bibomiale
# n = 5
# x = 2
# p = 6/18 = 1/3 = 0.3333333
ris_bin <- dbinom(2, 5, 6/18)
cat("Valore iniziale della binomiale =", ris_bin, "\n")

# Poisson
ris_poi <- dpois(2, 5 * 6/18)
cat("Valore iniziale della poisson =", ris_poi, "\n")

# Increase the n, the number of trials, and see what happens
ris_bin <- c()
ris_poi <- c()
vector <- seq(1, 40, 1)

for (n in vector) {
  ris_bin <- c(ris_bin, dbinom(2, n, 6/18))
  #  cat("Prob. Binom =", n, ":", ris_bin, "\n")
  ris_poi <- c(ris_poi, dpois(2, n * 6/18))
  #  cat("Prob. Poisson =", n, ":", ris_poi, "\n")
}

#ris_bin
#ris_poi

par(bg="white")
plot(ris_bin, type = "p", col = "red", ylim = c(0, 0.4),
     xlim = c(0, 40), xlab = "n", ylab = "Probabilità")
points(ris_poi, type = "p", col = "blue")
legend("topright", legend = c("Binomiale", "Poisson"),
       col = c("red", "blue"), pch = c(1, 1))



# ******************       Binomiale -> Normale       ***********************
#Consideriamo un'urna con 18 palline di cui 6 bianche e 12 nere.
#Estraiamo 5 palline senza reintroduzione.
#P( 2 bianche su 5 estrazioni) =?

# Bibomiale
# n = 5
# x = 2
# p = 6/18 = 1/3 = 0.3333333
ris_bin <- dbinom(2, 5, 6/18)
cat("Valore iniziale della binomiale =", ris_bin, "\n")
npq <- 50 * 6/18 * (1 - 6/18)
cat("Valore iniziale di npq =", npq, "\n")

# Normale
ris_norm <- dnorm(2, 5 * 6/18, sqrt(npq))
cat("Valore iniziale della normale =", ris_norm, "\n")

# Increase the n, the number of trials, and see what happens
# In order to use the normal distribution, we need to increase the number of trials
ris_bin <- c()
ris_norm <- c()
vector <- seq(1, 30, 1)

for (n in vector) {
  ris_bin <- c(ris_bin, dbinom(2, n, 6/18))
  #  cat("Prob. Binom =", n, ":", ris_bin, "\n")
  npq <- n * 6/18 * (1 - 6/18)
  ris_norm <- c(ris_norm, dnorm(2, n * 6/18, sqrt(npq)))
  #  cat("Prob. Normale =", n, ":", ris_norm, "\n")
}

#ris_bin
#ris_norm

par(bg="white")
plot(ris_bin, type = "o", col = "red", ylim = c(0, 0.4),
     xlim = c(0, 30), xlab = "npq", ylab = "Probabilità")
points(ris_norm, type = "o", col = "blue")
abline(v = 10, col = "red")
legend("topright", legend = c("Binomiale", "Normale"),
       col = c("red", "blue"), pch = c(1, 1))



# ******************       Poisson -> Normale       ***********************
#Consideriamo un'urna con 18 palline di cui 6 bianche e 12 nere.
#Estraiamo 5 palline senza reintroduzione.
#P( 2 bianche su 5 estrazioni) =?

# Poisson
ris_poi <- dpois(2, 5 * 6/18)
cat("Valore iniziale della poisson =", ris_poi, "\n")
cat("Valore iniziale di lambda =", 5 * 6/18, "\n")

# Normale
ris_norm <- dnorm(2, 5 * 6/18, sqrt(5 * 6/18))
cat("Valore iniziale della normale =", ris_norm, "\n")

# Increase the n, the number of trials, and see what happens
# In order to increase the lambda
ris_poi <- c()
ris_norm <- c()
vector <- seq(1, 30, 1)

for (n in vector) {
  ris_poi <- c(ris_poi, dpois(2, n * 6/18))
  #  cat("Prob. Poisson =", n, ":", ris_poi, "\n")
  ris_norm <- c(ris_norm, dnorm(2, n * 6/18, sqrt(n * 6/18)))
  #  cat("Prob. Normale =", n, ":", ris_norm, "\n")
}

#ris_poi
#ris_norm

par(bg="white")
plot(ris_poi, type = "o", col = "red", ylim = c(0, 0.4),
     xlim = c(0, 30), xlab = "lambda", ylab = "Probabilità")
points(ris_norm, type = "o", col = "blue")
abline(v = 10, col = "red")
legend("topright", legend = c("Poisson", "Normale"),
       col = c("red", "blue"), pch = c(1, 1))



# ******************       Chi-quadro -> Normale       ***********************
#Consideriamo un'urna con 18 palline di cui 6 bianche e 12 nere.
#Estraiamo 5 palline senza reintroduzione.
#P( 2 bianche su 5 estrazioni) =?

# Chi-square
ris_chi <- dchisq(2, 5 * 6/18)
cat("Valore iniziale della chi-square =", ris_chi, "\n")

# Normale
ris_norm <- dnorm(2, 5 * 6/18, sqrt(5 * 6/18))
cat("Valore iniziale della normale =", ris_norm, "\n")

# Increase r, the number of degrees of freedom, and see what happens

ris_chi <- c()
ris_norm <- c()
vector <- seq(1, 50, 1)

for (r in vector) {
  ris_chi <- c(ris_chi, dchisq(2, r * 6/18))
  #  cat("Prob. Chi-square =", r, ":", ris_chi, "\n")
  ris_norm <- c(ris_norm, dnorm(2, r * 6/18, sqrt(r * 6/18)))
  #  cat("Prob. Normale =", r, ":", ris_norm, "\n")
}

#ris_chi
#ris_norm

par(bg="white")
plot(ris_chi, type = "o", col = "red", ylim = c(0, 0.4),
     xlim = c(0, 50), xlab = "r", ylab = "Probabilità")
points(ris_norm, type = "o", col = "blue")
abline(v = 30, col = "red")
legend("topright", legend = c("Chi-square", "Normale"),
       col = c("red", "blue"), pch = c(1, 1))



# ******************       t di Student -> Normale       ***********************
degrees_of_freedom <- 10
x <- seq(1, 30, length.out = 100)

# Calcolo iniziale per Student's t e Normale
ris_t_iniziale <- dt(x, degrees_of_freedom)
ris_norm_iniziale <- dnorm(x, mean = degrees_of_freedom, sd = sqrt((2 * degrees_of_freedom) / (degrees_of_freedom - 2)))

cat("Valore iniziale della student's t =", ris_t_iniziale[1], "\n")
cat("Valore iniziale della normale =", ris_norm_iniziale[1], "\n")

# Aumentare i gradi di libertà e vedere cosa succede
ris_t <- list()
ris_norm <- list()
vector <- seq(3, 50, 1)  # Inizia da 3 per evitare divisione per zero

for (r in vector) {
  ris_t[[r]] <- dt(x, r)
  ris_norm[[r]] <- dnorm(x, mean = r, sd = sqrt(2 * r / (r - 2)))
}

# Creazione del grafico
par(bg = "white")
plot(x, ris_t[[3]], type = "p", col = "red", ylim = c(0, max(ris_t[[3]], ris_norm[[3]])), 
     xlim = c(1, 30), xlab = "x", ylab = "Densità")
lines(x, ris_norm[[3]], type = "p", col = "blue")
abline(v = 30, col = "red")
# for (i in 4:length(vector)) {
#     lines(x, ris_t[[i]], type = "l", col = "red")
#     lines(x, ris_norm[[i]], type = "l", col = "blue")
# }

legend("topright", legend = c("Student's t", "Normale"), col = c("red", "blue"), lty = 1)
