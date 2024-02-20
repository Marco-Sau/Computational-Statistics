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



# From the Textbook____________________________________________________________
# GGPLOT2 *********************************************************************
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