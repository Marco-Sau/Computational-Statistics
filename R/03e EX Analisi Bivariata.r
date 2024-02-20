# ANALISI BIVARIATA: Esercizio di Riepilogo
# 1 - Generare un dataset di 200 osservazioni con 3 variabili numeriche, 
# 1 variabile data, 
#     2 variabili factor e una variabile ordered.factor
#     per le variabili numeriche si può utilizzare la funzione rnorm, 
#     per i factor si può generare un vettore di stringhe 
#     e trasformarlo in factor

# 2 - produrre una tabella che riporti le prinicpali statistiche descrittive 
#     per ogni variabile

# 3 - produrre una tabella che riporti le correlazioni tra coppie 
#     di variabili

# 4 - stimare la relazione di causalità esistente tra due variabili numeriche

# 5 - misurare l'associazione tra coppie di variabili factor

# 6 - rappresentare graficamente le relazioni tra le variabili osservate 


# 1. Generating the Dataset ---------------------------------------------------
set.seed(123)  # for reproducibility

# Numerical variables
num_var1 <- rnorm(200)
num_var2 <- rnorm(200)
num_var3 <- rnorm(200)

# Date variable
date_var <- seq(as.Date("2023-01-01"), by="days", length.out=200)

# Factor variables
factor_var1 <- factor(sample(c("Level1", "Level2", "Level3"), 200, 
                             replace=TRUE))
factor_var2 <- factor(sample(c("CategoryA", "CategoryB", "CategoryC"), 200, 
                             replace=TRUE))

# Ordered factor variable
ordered_factor_var <- factor(sample(c("Low", "Medium", "High"), 200, 
                                    replace=TRUE), 
                             levels = c("Low", "Medium", "High"), 
                             ordered = TRUE)

# Combine into a data frame
dataset <- data.frame(num_var1, 
                      num_var2, 
                      num_var3, 
                      date_var, 
                      factor_var1, 
                      factor_var2, 
                      ordered_factor_var)
head(dataset)


# 2. Descriptive Statistics ---------------------------------------------------
summary(dataset)
str(dataset)


# 3. Correlation Matrix -------------------------------------------------------
correlations <- cor(dataset[, sapply(dataset, is.numeric)], method = "pearson")
correlations

correlations <- cor(dataset[, sapply(dataset, is.numeric)], method = "spearman")
correlations


# 4. Causal Relationship ------------------------------------------------------
causality_model <- lm(num_var1 ~ num_var2, data = dataset)
summary(causality_model)

residuals <- residuals(causality_model)
par(bg = "white")
plot(num_var1, num_var2)
as.numeric(lm(num_var1 ~ num_var2)$coefficients)
abline(a=as.numeric(lm(num_var1 ~ num_var2)$coefficients)[1],
       b=as.numeric(lm(num_var1 ~ num_var2)$coefficients)[2])

require(quantreg)
formula <- num_var1 ~ num_var2
residuals_quantile <- rq(formula, tau = 0.5)
abline(residuals_quantile, col = "red")


hist(residuals)

qqnorm(residuals)



causality_model <- lm(num_var1 ~ num_var3, data = dataset)
summary(causality_model)

residuals <- residuals(causality_model)
par(bg = "white")
plot(num_var1, num_var3)
as.numeric(lm(num_var1 ~ num_var3)$coefficients)
abline(a=as.numeric(lm(num_var1 ~ num_var3)$coefficients)[1],
       b=as.numeric(lm(num_var1 ~ num_var3)$coefficients)[2])

hist(residuals)



causality_model <- lm(num_var2 ~ num_var3, data = dataset)
summary(causality_model)

residuals <- residuals(causality_model)
par(bg = "white")
hist(residuals)



# 5. Association between Factor Variables --------------------------------------
# kendall's tau
cor(as.numeric(dataset$factor_var1), as.numeric(dataset$factor_var2), 
    method = "kendall")
# The two variable are not correlated

cor(as.numeric(dataset$factor_var1), as.numeric(dataset$ordered_factor_var), 
    method = "kendall")
# The two variable are not correlated

cor(as.numeric(dataset$factor_var2), as.numeric(dataset$ordered_factor_var), 
    method = "kendall")
# The two variable are not correlated


# chi-square test
assoc_test1 <- chisq.test(dataset$factor_var1, dataset$factor_var2)
assoc_test1

assoc_test2 <- chisq.test(dataset$factor_var1, dataset$ordered_factor_var)
assoc_test2

assoc_test3 <- chisq.test(dataset$factor_var2, dataset$ordered_factor_var)
assoc_test3


# deviance test
tbl <- xtabs(~factor_var1 + factor_var2, data = dataset)
tbl

frequenze_osservate <- tbl
frequenze_attese <- chisq.test(tbl)$expected

sum((frequenze_osservate - frequenze_attese)^2 / frequenze_attese)


# 6. Graphical Representation -------------------------------------------------
# Scatter plot for numerical variables
par(bg="white")
par(mfrow=c(1,3))
plot(dataset$num_var1, dataset$num_var2, 
     main="Scatter plot of Num_Var1 vs Num_Var2")
plot(dataset$num_var1, dataset$num_var3, 
     main="Scatter plot of Num_Var1 vs Num_Var3")
plot(dataset$num_var2, dataset$num_var3, 
     main="Scatter plot of Num_Var2 vs Num_Var3")

# barplot for factor variables
ftable_ <- ftable(table(dataset$factor_var1, dataset$factor_var2), 
                  dataset$ordered_factor_var)
par(mfrow=c(1,1))
par(bg="white")
# Bar plot for factor variables
barplot(table(dataset$factor_var1, dataset$factor_var2), legend = TRUE, 
        main = "Bar plot of Factor_Var1 vs Factor_Var2")

barplot(table(dataset$factor_var1, dataset$factor_var2), legend = TRUE, 
        main = "Bar plot of Factor_Var1 vs Factor_Var2",
        beside = TRUE)

# mosaic plot for factor variables
par(bg="white")
mosaicplot(table(dataset$factor_var1, dataset$factor_var2, 
                 dataset$ordered_factor_var), 
      main = "Mosaic plot of Factor_Var1 vs Factor_Var2 vs Ordered_Factor_Var")