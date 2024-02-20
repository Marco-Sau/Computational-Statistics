# ====================== MULTIVARIATE ANALYSIS ================================
rm(list=ls())

# Load data 
load("AcquistiOnLine.Rda")
str(AcquistiOnLine)


## 4.2 Working With Data Frames @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Data Lookup ***************************************************************
#### With function ------------------------------------------------------------
# funzioni su data.frame
# Calculate the mean of the variable "Spesa_media_mensile"
with(AcquistiOnLine, mean(Spesa_media_mensile))
mean(AcquistiOnLine$Spesa_media_mensile)

# set conditions, returns a vector of TRUE/FALSE
with(AcquistiOnLine, Spesa_media_mensile > 15)


### Modifying cells, columns, and names ***************************************
#### tolower function ---------------------------------------------------------
names(AcquistiOnLine)
# Transform a text from upper to lower case
tolower(names(AcquistiOnLine))


### The subset function *******************************************************
head(AcquistiOnLine)

# Subset of a table
? subset
subset(AcquistiOnLine, select = data_contatto:Area_geografica)

# ragioniamo per esclusione
subset(AcquistiOnLine, select = -c(data_contatto,Area_geografica))

# Using iteratively the subset function
subset(AcquistiOnLine, 
       subset = Area_geografica=="Nord",
       select = Livello_istruzione:Prezzo_acquisto)

# Within
? within
within(AcquistiOnLine, {Prezzo_acquisto = Prezzo_acquisto / 1000})
oggetto <- within(AcquistiOnLine, {Prezzo_acquisto = Prezzo_acquisto / 1000})
oggetto


### Transforming values *******************************************************
# Transform, same results as above
transform(AcquistiOnLine, Prezzo_acquisto = Prezzo_acquisto / 1000)


### Reshaping ****************************************************************
# Wide and long data
# Wide data
# dataframe di esempio
large_data <- data.frame(
  ID = c(1, 2, 3),
  Var1_T1 = c(10, 20, 30),
  Var1_T2 = c(15, 25, 35),
  Var2_T1 = c(100, 200, 300),
  Var2_T2 = c(150, 250, 350)
)

large_data

# Reshape da largo a lungo
? reshape
long_data <- reshape(large_data, idvar = "ID", 
                     varying = list(c("Var1_T1", 
                                      "Var1_T2", 
                                      "Var2_T1", 
                                      "Var2_T2")), 
                     direction = "long", timevar = "Time")


print(long_data)
#? Try to remane the columns in a more readable way

# Start from a long table and reshape it to a wide table
# dataframe di esempio
long_data <- data.frame(
  ID = rep(c(1, 2, 3), each = 2),
  Time = rep(c("T1", "T2"), times = 3),
  Var1 = c(10, 20, 15, 25, 30, 35),
  Var2 = c(100, 200, 150, 250, 300, 350)
)

long_data

# Reshape da lungo a largo
wide_data <- reshape(long_data, idvar = "ID", 
                     timevar = "Time", 
                     direction = "wide")

print(wide_data)


### Merging ******************************************************************
AcquistiOnLine
# Create 2 subsets of the dataset
AcquistiOnLine1<-AcquistiOnLine[1:15,1:4]
AcquistiOnLine2<-AcquistiOnLine[11:20,c(1,5:7)]

? merge
# Elementi comuni
merge(AcquistiOnLine1, AcquistiOnLine2, by.x="data_contatto", 
      by.y="data_contatto", all=FALSE)

# Tutti gli Elementi
merge(AcquistiOnLine1, AcquistiOnLine2, by.x="data_contatto", 
      by.y="data_contatto", all=TRUE)

# Tutti gli elementi del primo file più quelli comuni
merge(AcquistiOnLine1, AcquistiOnLine2, by.x="data_contatto", 
      by.y="data_contatto", all.x=TRUE)

# Tutti gli elementi del secondo file più quelli comuni
merge(AcquistiOnLine1, AcquistiOnLine2, by.x="data_contatto", 
      by.y="data_contatto", all.y=TRUE)


### Data cleaning *************************************************************
# cambio tipo di variabile

#as.character()
with(AcquistiOnLine, as.character(Area_geografica))

#as.numeric()
with(AcquistiOnLine, as.numeric(Genere))

#as.factor()
with(AcquistiOnLine, as.factor(Numero_pagine_visitate))

#as.logical()
#as.Date()



## 4.3 Applying a function over a collection @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Map ***********************************************************************
#### The aggregate function ---------------------------------------------------
? aggregate
aggregate(Prezzo_acquisto ~ Genere, data = AcquistiOnLine, summary)
with(AcquistiOnLine, summary(Prezzo_acquisto))

# Same as above, but using the split function
# Divide a vector in two vectors
with(AcquistiOnLine, split(Prezzo_acquisto, Genere))

# Stratify the data by two variables
aggregate(Prezzo_acquisto ~ Genere + Livello_istruzione, 
          data = AcquistiOnLine, summary)
aggregate(Prezzo_acquisto ~ Genere + Livello_istruzione + Area_geografica, 
          data = AcquistiOnLine, summary)

# let's check the variability (sd)
aggregate(Prezzo_acquisto ~ Genere + Livello_istruzione + Area_geografica, 
          data = AcquistiOnLine, sd)
oggetto <- aggregate(Prezzo_acquisto ~ 
                       Genere + Livello_istruzione + Area_geografica, 
                     data = AcquistiOnLine, sd)
str(oggetto) # data frame

# sort
? sort
sort(oggetto$Prezzo_acquisto) # order only one column

# order
? order
oggetto[order(oggetto$Prezzo_acquisto),] # order the whole dataframe
# reverse the order
# order the whole dataframe in descending order
oggetto[order(oggetto$Prezzo_acquisto, decreasing = TRUE),] 

# split
? split
sapply(with(AcquistiOnLine, split(Prezzo_acquisto, Genere)),
       summary)


#### The sapply function -----------------------------------------------------
sapply(AcquistiOnLine, mean)


#### The do.call function -----------------------------------------------------
?do.call
# La funzione do.call in R è utilizzata per valutare una 
# chiamata a una funzione con argomenti forniti come lista. 

# Possibile uso
# Creare una funzione personalizzata
funzione <- function(a, b, c) {
  data.frame(cbind(a,b,c))
}
funzione 

# Creare una lista di argomenti per la funzione
lista_argomenti <- list(a = AcquistiOnLine$Genere, 
                        b = AcquistiOnLine$Numero_pagine_visitate, 
                        c = AcquistiOnLine$Prezzo_acquisto_cat)

lista_argomenti
# Utilizzare do.call per chiamare la funzione con argomenti dalla lista
# The qualitative variables are converted to numerical variables
do.call(funzione, lista_argomenti)



### Filter ********************************************************************
# Show only the rows where the variables are factors
Filter(is.factor, AcquistiOnLine)


### Reduce ********************************************************************
# Apply a function to some columns of a dataframe
Reduce("+", 1:4)

# Utilizzare Reduce per calcolare la spesa media mensile per pagine 
# visitate (condizionata)
SpesaMediaXPaginaVisitata <- Reduce(function(x, y) ifelse(y>0, x/y, NA), 
                                    AcquistiOnLine[,c(8,10)])
# it's just an example, maybe it doesn't make sense to calculate the mean
SpesaMediaXPaginaVisitata



## 4.4 Using external data @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
### Web-based data sets *******************************************************
#### Execel files -------------------------------------------------------------
# Dati da fonti esterne

# R dispone di alcuni pacchetti aggiuntivi per interagire direttamente 
# con Excel: il pacchetto xlsx può leggere e scrivere file Excel 2007 
# (xlsx); il pacchetto gdata fornisce la funzione read.xls per leggere 
# file xls più vecchi; il framework RExcel (http://rcom.univie.ac.at/) 
# può integrare R con diverse versioni di Excel per Microsoft Windows; 
# e il pacchetto XLConnect offre funzionalità simili a xlsx.

# RStudio permette di importare file .csv utilizzano un apposito menù

# Importazione di file xls da internet. Esempio:
#install.packages("gdata")
#install.packages("readxl")
require("gdata")                        # must be installed
library(readxl)                       # must be loaded
#f <- "http://www.eia.gov/petroleum/gasdiesel/xls/pswrgvwall.xls"
f <- "13/pswrgvwall.xls"
? read_xls
gas_prices <- read_xls(f, sheet=2, skip=2) # non importa le prime 2 righe
str(gas_prices)
gas_prices <- setNames(gas_prices[,1:2], c("Date", "Weekly_US"))
gas_prices[1:10,]

#? Sys.setlocale 
# Change the local language to english 
#Sys. setlocale("LC_TIME", "en_US.UTF-8")

# Convert the Date column to Date type
# Since your dates are in the format 'YYYY-MM-DD HH:MM:SS', use '%Y-%m-%d'
gas_prices$Date <- as.Date(as.character(gas_prices$Date), format = "%Y-%m-%d")

# Check the converted dates
print(head(gas_prices$Date))

# Plotting the data
plot(Weekly_US ~ Date, data = gas_prices, type = "l")


#### The quandl package -------------------------------------------------------

# Quandl.com è un sito web che indicizza dati in serie temporali 
# provenienti da numerose fonti. Dispone di milioni di set di dati e, 
# ancor meglio, di un'interfaccia aperta per scaricare (e caricare) dati. 
# Il pacchetto Quandl fornisce un'interfaccia agli utenti di R. 
# Per scaricare un file è semplice come navigare nel sito per trovare 
# i dati di interesse e notare il codice che Quandl assegna.

# Esempio

#install.packages("Quandl")
#require(Quandl)

# Doesn't exist anymore
#ch_0014 <- Quandl("WORDBANK/CHN_SP_POP_0014_TO_ZS")

# Stringa nel formato "Aug 20, 1990"
stringa_data <- "Aug 20, 1990"

# Trasformare la stringa in una data
data <- as.Date(stringa_data, format = "%b %d, %Y")

# Stampare il risultato
print(data)





### MULTIVARIATE GRAPHICS @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ====================== MULTIVARIATE GRAPHICS ================================
# rm(list=ls())
## 5.1 Base graphics @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### Scatterplot --------------------------------------------------------------
with(iris,
     plot(Sepal.Length, Sepal.Width,
          pch=as.numeric(Species), cex=1.2))
legend(6.1, 4.4, c("setosa", "versicolor", "virginica"),
       cex=0.75, pch=1:3)

library(UsingR)
str(Cars93)

Cars93 <- transform(Cars93, price = cut(Price, c(0,15,30,75),
                                        labels = c("cheap", "affordable", "expensive")))   
Cars93$price

plot(MPG.highway ~ Weight, Cars93, pch = as.numeric(price))
legend(3500, 50, levels(Cars93$price), pch=1:3)

l <- split(Cars93, Cars93$price)
l

## 3 graphics in one
par(mfrow=c(1,1))

## common to each graphic
fm <- MPG.highway ~ Weight
xlim <- range(Cars93$Weight)
ylim <- range(Cars93$MPG.highway)
plot(0,0, type="n", xlim=xlim, ylim=ylim, xlab="Weight", ylab="MPG.highway")
for (i in 1:length(l)) {
  points(fm, data=l[[i]], pch=i)
  abline(lm(fm, data=l[[i]]), col=i)
}


#### Bubble charts ------------------------------------------------------------
par(mfrow=c(1,1))
head(SAT)
plot(total ~ salary, data=SAT, cex=sqrt(perc/10), pch=16,
     col=rgb(red=0, green = 0, blue = 0, alpha = 0.250))

# Same with colored bubbles
par(mfrow=c(1,1))
head(SAT)
# Generate a color palette based on some variable (e.g., 'perc')
colors <- rainbow(length(SAT$perc))
# Create the scatter plot with colored bubbles
plot(total ~ salary, data=SAT, cex=sqrt(perc/10), pch=16, col=colors)


#### Pairs plots (scatterplot matrix) -----------------------------------------
species <- iris$Species
values <- Filter(is.numeric, iris)
pairs(values, col=species)


#### Parallel coordinates plots -----------------------------------------------
x <- state.x77 
x
case <- "New York"
ind <- rownames(x) == case
parcoord(state.x77,
         col=gray(c(0.8, 0.2))[ind+1],
         lwd=(1:2)[ind + 1], las = 2)


#### Heatmap matrix -----------------------------------------------------------
? heatmap
x <- sapply(as.data.frame(state.x77), rank)
rownames(x) <- rownames(state.x77)
heatmap(x, Rowv=NA, Colv=NA,
        scale="column", # scale le colonne
        margins=c(8, 6), # spazio per le etichette
        col=rev(gray.colors(50)))

#install.packages("viridis")
library(viridisLite)
heatmap(x, Rowv=NA, Colv=NA, # Dendrogram
        scale="column", # scale le colonne
        margins=c(8, 6), # spazio per le etichette
        col=viridis(50))



## 5.2 Lattice graphics @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#### xyplot -------------------------------------------------------------------
library(lattice)
xyplot(MPG.highway ~ Weight | Price, data=Cars93) # causal relationship

# Create a new variable called price that is a factor with three levels
Cars93 = transform(Cars93, price=cut(Price, c(0, 15, 30, 75),
                                     labels = c("cheap", "afforfdable",
                                                "expensive")))
xyplot(MPG.highway ~ Weight | price, data=Cars93)

# stessa frequenza
# equal count
prices <- equal.count(Cars93$Price, number=3, overlap=0)
# ref: normalized histogram 
# divide the frequency by the frequency density
# the frequency density is the frequency divided by the class width

?xyplot
xyplot(MPG.highway ~ Weight | prices, data=Cars93,
       layout=c(3,1),
       type=c("p", "r") # p: points, r: regression line
)


### Dot charts ***************************************************************
#### dotplot ------------------------------------------------------------------
? babies
head(babies)
str(babies)
str(unique(babies$smoke))
dotplot(factor(smoke) ~ wt, data=babies, subset=wt < 999 & ded==3)
# wt: weight in grams
# smoke: 0=non-smoker, 1=smoker, 9=unknown

# distribuzioni condizionate
dotplot(factor(smoke) ~ wt | factor(ded),
        data=babies, subset=wt < 999)

### Boxplots *****************************************************************
bwplot(factor(smoke) ~ wt, data=babies, subset=wt < 999)


### Histograms ****************************************************************
histogram( ~ wt | factor(smoke), data=babies, subset=wt < 999)


### Density plots *************************************************************
densityplot( ~ wt | factor(smoke), data=babies, subset=wt < 999)



## 5.3 The ggplot2 package @@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#? Slides sections_____________________________________________________________
### Data Visualization *********************************************************
#install.packages("dplyr")
library(dplyr) 
# Following ggplot2 cheat sheet
# https://rstudio.github.io/cheatsheets/html/data-visualization.html
library(ggplot2)


### Components of a ggplot2 graph *********************************************
library(readr)
murders <- read_csv("14/murders.csv") 
head(murders)

murders <- murders[,-1]
murders[1:3,]

theme_set(theme_grey()) # grey color theme
p <- ggplot(data = murders) # create a ggplot2 object
class(p)
p

# same results as above
murders %>% ggplot() # pipe operator


### Geometry Elements *********************************************************
#### Aesthetic Mappings -------------------------------------------------------
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total)) 
# y = total is the total murders

# alternative
p + geom_point(aes(population/10^6, total))

# update p
#p <- p + geom_point(aes(population/10^6, total))


#### Layers -------------------------------------------------------------------
p + geom_point(aes(population/10^6, total)) + 
  geom_text(aes(population/10^6, total, label = abb))
# geom_text adds labels to the points

# alternative
p_test <- p + geom_text(aes(population/10^6, total, label = abb))

#?ggplot2
### Details Modifications ******************************************************
# make the points bigger
p + geom_point(aes(population/10^6, total), size = 3) + 
  geom_text(aes(population/10^6, total, label = abb))

# nudge_x moves the labels to the right
# local aesthetic mapping
p + geom_point(aes(population/10^6, total), size = 3) + 
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 1.5)

#### Global and local aesthetics mappings --------------------------------------
# create a global aesthetic mapping
args(ggplot)
p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

p + geom_point(size = 3) + 
  geom_text(nudge_x = 1.5)

# create a local aesthetic mapping that excludes the global one
p + geom_point(size = 3) +
  geom_text(aes(x = 10, y = 800, label = "Hello there!"))


#### Scales of measurement ------------------------------------------------------
# logarithmic scale
p + 
  geom_point(size = 3) + 
  geom_text(nudge_x = 0.05) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10")

# alternative
p + 
  geom_point(size = 3) + 
  geom_text(nudge_x = 0.05) + 
  scale_x_log10() + 
  scale_y_log10()


#### Titles and labels ---------------------------------------------------------
p + geom_point(size = 3) +
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") + 
  ggtitle("US Gun Murders in 2010")


#### Coloring observations groups ----------------------------------------------
p <- murders %>% ggplot(aes(population/10^6, total, label = abb)) + 
  geom_text(nudge_x = 0.05) +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Populations in millions (log scale)") + 
  ylab("Total number of murders (log scale)") + 
  ggtitle("US Gun Murders in 2010")

# color argument
p <- p + geom_point(size = 3, color ="blue")
p

# color by region
p <- p + geom_point(aes(col=region), size = 3) 
p


#### Additional elements -------------------------------------------------------
# calcoliamo per ogni stato il tasso di criminalita'
r <- murders %>%    
  summarize(rate = sum(total) / sum(population) * 10^6) %>% 
  pull(rate) # r è l'intercetta
r # returns a float number

# aggiungiamo la retta di regressione
p <- p + 
  geom_point(aes(col=region), size = 3) + 
  geom_abline(intercept = log10(r))
p


p <- p + 
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col=region), size = 3) 
p

# change the legend layout
p <- p + 
  scale_color_discrete(name = "Region") 
p


#? Textbook sections___________________________________________________________
#### Aeshetics ----------------------------------------------------------------
### Geoms *********************************************************************
library(ggplot2)
p <- ggplot(Cars93)
p
p <- p + aes(x=Weight, y = MPG.highway, color=Origin)
p
aes(x=sqrt(Weight/1000), y=MPG.highway, color=Origin)
geom_point(cex=3)

p <- ggplot(Cars93, aes(x=sqrt(Weight/1000),
                        y=MPG.highway, color=Origin)) + geom_point(cex=3)
p

p <- ggplot(Cars93, aes(x=Cylinders, y=MPG.highway))

p + geom_boxplot()

p + geom_boxplot() + geom_jitter(position=
                                   position_jitter(w = 0.1), alpha = .25)

p + geom_boxplot() +
  geom_jitter(position=position_jitter(w = 0.1), alpha = .25)
### Grouping ******************************************************************
### Statistical transformations ***********************************************
#### stat_bin -----------------------------------------------------------------
data("morley")
summary(morley)
head(morley)
m <- subset(morley, Expt %in% 1:2)      # just first two experiments
m
p <- ggplot(m, aes(x=Run, y=Speed, group=Expt, color=Expt))
p
p + geom_line()                         # connect x and y with lines
p<-ggplot(Cars93, aes(x = MPG.highway))
p
p + geom_histogram()


#### stat_density -------------------------------------------------------------
p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(binwidth=5)
p

p <- ggplot(Cars93, aes(x=MPG.highway, y=..density..)) # scale y
p + geom_histogram(alpha=0.5) + geom_density()
dim(Cars93)
p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=15) + geom_density()
p

dim(Cars93)
p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=7) + geom_density()
p

p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=.7) + geom_density()
p

p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=1.5) + geom_density()
p


#### stat_smooth --------------------------------------------------------------
p <- ggplot(Cars93, aes(x=Weight, y=MPG.highway)) + geom_point()
p + geom_smooth()
p
p + geom_smooth(method="lm", se=FALSE)
p + geom_smooth(method="lm", se=TRUE)
### Faceting ******************************************************************
# Segmentazione
library(HistData)
data("PearsonLee")
summary(PearsonLee)
? PearsonLee
head(PearsonLee)
p <- ggplot(PearsonLee, aes(y=child,x=parent))
p
p + geom_point(alpha=0.5) + geom_smooth(
  method="loess") + facet_grid(par ~ chl)
summary(PearsonLee)


#### Margins ------------------------------------------------------------------
p <-  ggplot(PearsonLee, aes(y=child,x=parent))
p + geom_point(alpha=0.5) + geom_smooth(
  method="loess") + facet_grid(par ~ chl, margins = "chl")


#### Wrap ---------------------------------------------------------------------
# Distribuzioni condizionate
p <- ggplot(morley, aes(x=Speed)) + geom_histogram(
  binwidth=50)
p + facet_wrap( ~ Expt)



### From the textbook *********************************************************
### GGPLOT2 *******************************************************************
p <- ggplot(Cars93)
p
p <- p + aes(x=Weight, y = MPG.highway, color=Origin)
p
aes(x=sqrt(Weight/1000), y=MPG.highway, color=Origin)
geom_point(cex=3)

p <- ggplot(Cars93, aes(x=sqrt(Weight/1000),
                        y=MPG.highway, color=Origin)) + geom_point(cex=3)
p

p <- ggplot(Cars93, aes(x=Cylinders, y=MPG.highway))

p + geom_boxplot()

p + geom_boxplot() + geom_jitter(position=
                                   position_jitter(w = 0.1), alpha = .25)

p + geom_boxplot() +
  geom_jitter(position=position_jitter(w = 0.1), alpha = .25)

data("morley")
summary(morley)
head(morley)
m <- subset(morley, Expt %in% 1:2)      # just first two experiments
m
p <- ggplot(m, aes(x=Run, y=Speed, group=Expt, color=Expt))
p
p + geom_line()                         # connect x and y with lines


p<-ggplot(Cars93, aes(x = MPG.highway))
p
p + geom_histogram()

p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(binwidth=5)
p

p <- ggplot(Cars93, aes(x=MPG.highway, y=..density..)) # scale y
p + geom_histogram(alpha=0.5) + geom_density()

dim(Cars93)
p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=15) + geom_density()
p

dim(Cars93)
p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=7) + geom_density()
p

p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=.7) + geom_density()
p

p <- ggplot(Cars93, aes(x = MPG.highway,
                        y=..density..)) + stat_bin(
                          binwidth=1.5) + geom_density()
p

p <- ggplot(Cars93, aes(x=Weight, y=MPG.highway)) + geom_point()
p + geom_smooth()
p
p + geom_smooth(method="lm", se=FALSE)
p + geom_smooth(method="lm", se=TRUE)


# Segmentazione
data("PearsonLee")
summary(PearsonLee)
? PearsonLee
head(PearsonLee)
p <- ggplot(PearsonLee, aes(y=child,x=parent))
p
p + geom_point(alpha=0.5) + geom_smooth(
  method="loess") + facet_grid(par ~ chl)
summary(PearsonLee)

p <-  ggplot(PearsonLee, aes(x=child,y=parent))
p + geom_point(alpha=0.5) + geom_smooth(
  method="loess") + facet_grid(chl ~ par, margins = "chl")


# Distribuzioni condizionate
p <- ggplot(morley, aes(x=Speed)) + geom_histogram(
  binwidth=50)
p + facet_wrap( ~ Expt)