#### ======= ALL LECTURES ========
#### ======= Lecture 1 & 2 ========================================================
2+2

a <- 2^15

a = 2^15

# _a <-2^15

a_b <-2^15

# .2<-"pippo"

.a2<-"pippo"

.A2<-"pippo"

.A2
.a2

seq(1,9,2)

seq(1,9)


x<-seq(10^2,9^5,2)

x[10]

y<-c(100, 50, 125:130)

z<-c(seq(1,100,10),c(190,200), rep(1000, 10))

rep((1:10),10)


1:10 +1000

#z2<-z[11:19[-c(15,17)]]


length(z)

z * z2



media<-function(x){
  m<-sum(x)/length(x)
  return(m)
}

media(x)

varianza<-function(x){
  n<-length(x)
  m<-media(x)
  v<-sum((x-m)^2)/n
  return(v)
}

varianza(x)


dt <- 0.005
t <- seq(0, pi/6, by = dt)
ft <- cos(t)
(I <- sum(ft)*dt)



integrale<-function(x,y,f,dt){
  t<-seq(x,y,dt)
  ft<-sapply(t, f)
  I<-sum(ft)*dt
  return(I)
}

integrale(0, pi/6, cos, 0.005)

? apply


x<-seq(10,200,10)
f <- function(x){
  r <- (1 + 1/x)^x
  return(r)
}
plot(x, f(x), type="b")

x<-c(2:5,NA,10)
x
is.na(x)


y<-c(2:5,NULL,10)

# x == y


x[x %% 4 == 0]

? subset

subset(x, x>2)
x[x>2]

which(x>2)



x<-10
y<-10


if(x==10 & y==10) z<-y+y
z

x==10 & y==10


x<-1:10
y<-1:10
z<-1:10

x==y 

x[3]==3 & y[3]==3

x[4]==3 & y[3]==3

x[4]==3 && y[3]==3


(x & y)==z

(x && y)==z

#### Matrici

matrix(x, 5,2)

matrix(x, 5,2, byrow = T)

matrix(x)

matrix(10)

matrix(x, 2,3)

m<-matrix(x, 7,8)

dim(m)
nrow(m)
ncol(m)

diag(m)

diag(1:16, 4, 4)

m

rbind(m, matrix(1:16,2,8))


cbind(m, matrix(1:21,7,3))

m*m^2

m %*% t(m)

det(m)

m[,4]

det(m[,-ncol(m)])

m[3]
m[3,1]

is.matrix(m)
is.vector(m)

as.matrix(x)
as.matrix(x) == x

as.vector(m)
is.vector(m)


array(m, dim = c(7,4,2))

letters

dim(as.array(letters)) [26] 

dim(letters)

length(letters)


array(letters[1:24], c(2,4,3))

array(letters, c(2,4,3))



array(letters, c(2,4,4))

#array(letters, c(2,4,4), byrow=T)

ls()
objects()
rm(m2)
rm(list = ls())

# Exercises
# create un vettore di 100 elementi
# che non sono divisibili per 2, 3 o 7
x <- 1:100
y <- x[!x %% 2 == 0 & !x %% 3 == 0 & !x %% 7 == 0]
y

x = c('s', 'r', 'a', 'l')
x
x <- x[-1]
x
x <- c('b', x)
x
# remove 'a'
x <- x[- which(x == 'a')]
x

?diag
idenity_matrix <- diag(5, 10)
idenity_matrix


# Calculate the sum
mat <- matrix(1:10, 2, 5)
mat
# sum by column
col_sum <- colSums(mat)
col_sum
# sum by row
rowSums(mat)

# now try using apply
?apply
apply(mat, 1, sum) # sum by row
apply(mat, 2, sum) # sum by column


# Create a time table matrix
time_table <- matrix(1:10, 10, 10, byrow = F)
time_table
final <- time_table * t(time_table)
final



#### ======= Lecture 3 ========================================================
#### Exercise 1:---------------------------------------------------------------
# calcolare le radici reali (soluzioni) di un'equazione di secondo grado

# Code without function
soleq2 <- function(a, b, c){
  if(is.numeric(a) == FALSE || 
     is.numeric(b) == FALSE || 
     is.numeric(c) == FALSE){
    cat("One of the input is not numeric")
    break()
  }
  x1 = (sqrt(b^2 - 4 * a * c))
  x2 = (2 * a) 
  
  sol1 <- (-b + x1) / x2
  sol2 <- (-b - x1) / x2
  
  risults <- c("root1" <- sol1, "root2" <- sol2)
  cat("Solutions are:", "x1=", sol1, "x2=", sol2)
  return(risults)
}

result_exercise_1 = soleq2(1, 4, 2)
result_exercise_1


#### Exercise 2: -------------------------------------------------------------
# calcolare le radici reali (soluzioni) di un'equazione di secondo grado
# utilizzando if else
soleq2 <- function(a, b, c){
  if(is.numeric(a) == FALSE || 
     is.numeric(b) == FALSE || 
     is.numeric(c) == FALSE){
    cat("One of the input is not numeric")
    break()
  }
  
  if(length(a) > 1 || length(b) > 1 || length(c) > 1){
    cat("Arguments must be integers")
    break()
  }
  
  # Calculate the discriminant and use shortcut solutions
  discriminant <- b^2 - (4 * a * c)
  
  if(discriminant > 0){
    x1 = (sqrt(b^2 - (4 * a * c)))
    x2 = (2 * a) 
  } else if (discriminant == 0){
    sol1 <- -b / (2 + c)
  } else if (discriminant < 0){
    cat ("There are no rea solutions!")
  }
  
  sol1 <- (-b + x1) / x2
  sol2 <- (-b - x1) / x2
  
  risults <- c("root1" <- sol1, "root2" <- sol2)
  cat("Solutions are:", "x1=", sol1, "x2=", sol2)
  return(risults)
}

result_exercise_2 = soleq2(1, 4, 2)
result_exercise_2


soleq2<-function(a0,a1,a2){
  if(is.numeric(a0)==F || is.numeric(a1)==F || is.numeric(a2)==F){
    cat("Almeno uno degli argomenti non è numerico")
    break()}
  
  if(length(a0)>1 || length(a1)>1 || length(a2)>1){
    cat("I tre argomenti devono essere tre scalari")
    break()}
  
  x1<- sqrt(a1^2-4*a2*a0)
  x2<-2*a2
  
  sol1<- (-a1 + x1)/x2
  sol2<-  (-a1 - x1)/x2
  
  ris<-c("radice1"<- sol1, "radice2"<- sol2)
  cat("Le soluzioni sono: ", "a1 = ", sol1, "a2 = ", sol2)
  return(ris)
}

soleq2(2,NA,1)

vec<-c(1,2)
soleq2(vec,4,1)

xx<-soleq2(2,4,1)
xx

soleq2(4,5,6)

soleq2<-function(a0,a1,a2){
  if(is.numeric(a0)==F || is.numeric(a1)==F || is.numeric(a2)==F){
    cat("Almeno uno degli argomenti non è numerico")
    break()}
  
  if(length(a0)>1 || length(a1)>1 || length(a2)>1){
    cat("I tre argomenti devono essere tre scalari")
    break()}
  
  discr<-a1^2-4*a2*a0
  
  if(discr > 0){
    x1<- sqrt(a1^2-4*a2*a0)
    x2<-2*a2
    
    sol1<- (-a1 + x1)/x2
    sol2<-  (-a1 - x1)/x2
  } else if(discr == 0){
    sol1 <- -a1/(2+a2)
  } else if(discr < 0)
    sol1<-sol2<-NULL
  
  ris<-c("discriminante" <- discr, radice1<- sol1, "radice2"<- sol2)
  cat("Discriminente = ", discr, "; ", "Le soluzioni sono: ", "a1 = ", sol1, "a2 = ", sol2)
  return(ris)
}

xx<-soleq2(2,4,1)
xx

xx<-soleq2(4,5,6)
xx

i<-1:10; y<-1:100
for(i in y) print(i+y)


#### Cycles with for ---------------------------------------------------------
x <- 1:10
y <- 1:5
for(i in y){
  print(x+y)
}
#### Exercise 3: -------------------------------------------------------------
# Task 3: scrivere una funzione che somma gli elementi di un vettore e stampa, 
# ad ogni passo, l'indice del ciclo e il valore della somma

sum <- function(x){
  elements_sum <- 0
  for (i in 1:length(x)){
    elements_sum <- elements_sum + x[i]
    cat("Iteration", i, "sum = ", elements_sum, "\n")
  }
  return(elements_sum)
}
sum(1:100)


#### Exercise 4: -------------------------------------------------------------
### Task 4: Task 4: scrivere una funzione che calcola il valore di 
#un fondo pensione che fornisce un interesse annuo dell'11%, 
#ha una durata di 10 anni, si rivaluta mensilmente, 
#e prevede il versamento di 100 euro ogni mese.
#Rappresentare graficamente l'andamento del fondo
# interesse annuo = 11%
# durata = 10 anni
# quota = 100 euro mensili
# numero di pagamenti = 10 / 12

# Input 
tasso <- 0.11 # tasso di interesse annuo 
durata <- 10 # Durata (in anni) 
frazione <- 1/12 # Frazione annua periodo di rivalutazione 
versamento <- 100 # Somma versata in ogni periodo

# Calculus
n <- floor (durata/frazione) # n. di pagamenti 
fondo <- 0 # valore iniziale 
for (i in 1:n) { 
  fondo[i+1] <- fondo[i]* (1 + tasso*frazione) + versamento
}
periodo <- (0:n) *frazione

# Output 
plot (periodo, fondo)

## Create a function from this script
fondo_pensione <- function(tasso, durata, periodo, quota){
  frazione <- 1 / periodo
  
  n <- floor (durata/frazione) # n. di pagamenti 
  fondo <- 0 # valore iniziale 
  for (i in 1:n) { 
    fondo[i+1] <- fondo[i]* (1 + tasso*frazione) + quota
  }
  periodo <- (0:n) *frazione
  
  # Output 
  plot (periodo, fondo)
  return("Valore del fondo:" <- fondo)
}

fondo_pensione(.08, 20, 4, 500)


rest_prestito <- function(tasso,periodo, debito_iniziale, quota){
  frazione<-1/periodo
  tempo <- 0
  debito <- debito_iniziale # valore iniziale 
  while (debito > 0) {
    tempo <- tempo + frazione
    debito <-  debito*(1 + tasso*frazione) - quota
    print(c(round(tempo,2), round(debito,2)))
  }
  # Output
  cat("Il prestito sarà restituito in ", tempo, "anni", "\n")  
  return("tempo_restitu<"<-tempo)
}


rest_prestito(.15, 3, 2225, 1000)
rest_prestito(.14, 3, 10000, 1000)


#### While cycles ------------------------------------------------------------
#### Exercise 5: -------------------------------------------------------------
### Task 5: scrivere una funzione che calcola il tempo di restiruzione 
#di un prestito di 1000 euro a un tasso di interesse debitorio annuo 
#dell'11% e prevede versamenti mensili di 12 euro

periodo_di_restituzione <- function(tasso, periodo, debito_iniziale, quota){
  frazione <- 1 / periodo
#  n <- floor (durata/frazione)
  tempo <- 0
  debito <- debito_iniziale
  while (debito > 0){
    tempo <- tempo + frazione
    debito <- debito * (1 + tasso * frazione) - quota
  }
  cat("Il prestito sarà estinto in", tempo, "anni", "\n")
  return("Tempo di restituzione:" <- tempo)
}

periodo_di_restituzione(.11, 12, 1000, 12)
# .11 = tasso di interesse debitorio annuo
# 12 = periodo
# 1000 = debito iniziale
# 12 = quota


#### Programmazione vettoriale ------------------------------------------------
a <- c(1:10)
a
b <- 100:105
b
# If a is greater than 5 print a, else print b
ifelse(a > 5, a, b)

# Sum of the first n squared numbers
# Without cycle
n <- 100
s <- 0
for (i in 1:n)
  s <- s + i^2
s

# With cycle
sum((1:n)^2)

# Example
x <- c(-2, -1, 1, 2)
ifelse(x > 0, "Positive", "Negative")

# Example
x <- c(10, 20, 5, 30, 15)
constrain <- x > 15
ifelse(constrain, x^2, x^3)

# pmin and pmax functions
pmin(c(1, 2,3) , c(3, 2,1), c(2,2,2))
pmax(c(1, 2,3) , c(3, 2,1), c(2,2,2))

#### Exercise 6: -------------------------------------------------------------
# Task 6: definire due vettori, a e b, rispettivamente di 10 e 5 elementi, 
# e valutare ciascun elemento di a rispetto alla condizione a > 5. 
# Se la condizione é soddisfatta, uno specifico elemento viene selezionato 
#dal vettore a, altrimenti viene selezionato dal vettore .

a <- c(1:10)
b <- 100:105
ifelse(a > 5, a, b)

#### ===== Lecture 4 =========================================================
#--- Lecture 4 ---
rm(list = ls())
#### Exercise 5: -------------------------------------------------------------
### Task 5 modified: scrivere una funzione che calcola il tempo di restiruzione 
#di un prestito di 1000 euro a un tasso di interesse debitorio annuo 
#dell'11% e prevede versamenti mensili di 12 euro
periodo_di_restituzione <- function(tasso, periodo, debito_iniziale, quota){
  frazione <- 1 / periodo
  #  n <- floor (durata/frazione)
  tempo <- 0
  debito <- debito_iniziale
  while (debito > 0){
    tempo <- tempo + frazione
    debito <- debito * (1 + tasso * frazione) - quota
    # The following condition avoid to print negative values
    if (debito < 0){
      debito <- 0
    }
    print(c(round(tempo, 2), round(debito, 2)))
  }
  cat("Il prestito sarà estinto in", tempo, "anni", "\n")
  return("Tempo di restituzione:" <- tempo)
}

periodo_di_restituzione(.11, 12, 1000, 12)



#### Programmazione vettoriale ------------------------------------------------
a <- c(1:10)
a
b <- 100:105
b
# If a is greater than 5 print a, else print b
ifelse(a > 5, a, b)

# Sum of the first n squared numbers
# Without cycle
n <- 100
s <- 0
for (i in 1:n){
  s <- s + i^2
}
s

# With cycle
sum((1:n)^2)

# Example
x <- c(-2, -1, 1, 2)
ifelse(x > 0, "Positive", "Negative")

# Example
x <- c(10, 20, 5, 30, 15)
constrain <- x > 15
ifelse(constrain, x^2, x^3)

# pmin and pmax functions
pmin(c(1,2,3) , c(3,2,1), c(2,2,2))
pmax(c(1, 2,3) , c(3,2,1), c(2,2,2))

#### Exercise 6: -------------------------------------------------------------
### Task 6: definire due vettori, a e b, rispettivamente di 10 e 5 elementi, 
#e valutare ciascun elemento di a rispetto alla condizione a > 5. 
#Se la condizione é soddisfatta, uno specifico elemento viene selezionato 
#dal vettore a, altrimenti viene selezionato dal vettore b.

a <- c(1:10)
b <- c(15:20)
ifelse(a > 5, a, b)


#### Pseudocodifica ----------------------------------------------------------
# Example
x <- 3
for (i in 1:3){
  show(x)
  if (x %% 2 == 0){
    x <- x / 2
  } else {
    x <- 3 * x + 1
  }
}
show(x)


#### Debugging ---------------------------------------------------------------
# Task 7: Si definisca un oggetto x che contiene il valore 3. 
#Si generi un ciclo da 1 a 3 e, in ogni iterazione i, si ponga 
#x = x/2 se x é divisibile per 2, altrimenti × = 3x + 1

x <- 3
for (i in 1:3){
  cat("At iteration", i, "the value of x is:", x, '\n')
  if(x %% 2 == 0){
    cat("Iteration", i, "satisfy the first condition", "\n\n")
    x <- x / 2
  } else{
    cat("Iteration", i, "satisfy the second condition", "\n\n")
    x <- 3 * x + 1
  }
}


# Example of use of the warning function
# In this case the process stops
options(warn = 2)  # Turn warnings into errors

x <- 3
for (i in 1:3){
  if (x < 0) {
    warning("x has become negative at iteration ", i)
  }
  if (x > 1000) {
    warning("x is too large at iteration ", i)
  }
  
  if (x %% 2 == 0) {
    x <- x / 2
  } else {
    x <- 3 * x + 1
  }
}






#### Exercises ---------------------------------------------------------------
# Exercise 1: Write a function and plot the results --------------------------
function_1 <- function(x){
  if (x <= 0){
    y <- -x^3
  } else if(x > 0 && x <= 1){
    y <- x^2
  } else {
    y <- sqrt(x)
  }
  return(y)
}

input_1 <- seq(-2, 2, 0.1)
output_1 <- vector()
for (i in 1 : length(input_1)){
  output_1[i] <- function_1(input_1[i])
}

plot(input_1, output_1, 
     type = "l", 
     xlab = "Input", 
     ylab = "Output", 
     main = "Exercise 1", 
     col="blue")


# Exercise 2a: Write a function using a for cycle, while cycle and no cycles.---
x1 <- 0.3
n1 <- 55
x2 <- 6.6
n2 <- 8


function_2a <- function(x, n){
  result <- 0
  for (i in 0: n){
    result <- result + x^i
  }
  return(result)
}

output_2a <- function_2a(x1, n1)
output_2b <- function_2a(x2, n2)
output_2a
output_2b

# 2b
# 2b
function_2b <- function(x, n){
  result <- 0
  if (x == 1){
    result <- n + 1
  } else {
    result <- (1 - x^(n + 1)) / (1 - x)
  }
  return(result)
}

output_2a <- function_2b(x1, n1)
output_2b <- function_2b(x2, n2)
output_2a
output_2b

# 2c: write the first function using a while loop
function_2_while <- function(x, n){
  result <- 0
  counter <- 0
  while (counter <= n){
    result <- result + x^counter
    counter <- counter + 1
  }
  return(result)
}

output_2a <- function_2_while(x1, n1)
output_2b <- function_2_while(x2, n2)
output_2a
output_2b

# 2d: write the first function with vectorial operations
function_2_no_cycle <- function(x, n){
  result <- 0
  result <- sum(x^(0:n))
  return(result)
}

output_2a <- function_2_no_cycle(x1, n1)
output_2b <- function_2_no_cycle(x2, n2)
output_2a
output_2b

# Exercise 3 -----------------------------------------------------------------
my_vector <- c(1:10)
n <- 10 
## Arithmetic average
ma <- mean(my_vector)
ma

## Geometric average
# With for

mg <- function(x, n){
  result <- 1
  for (i in 1:n){
    result <- (result * x[i])
  }
  result <- result ^ (1 / n)
  return(result)
}

test <- mg(my_vector, n)
test

# With vectors
mg <- prod((my_vector))^(1/length(my_vector))
mg

## Harmonic averate with vectors
mh <- (sum(1 / my_vector) ^ (-1)) * length(my_vector)
mh

# Verify the following relationship: ma ≥ mg ≥ mh
(ma >= mg) & (mg >= mh)



# Exercise 4 -----------------------------------------------------------------
# Calculate the average of the elements in positions multiple of 3
my_vector <- sample(100, 12)
my_vector

function_4 <- function(x){
  result <- 0
  for (i in 1:length(x)){
    if (i %% 3 == 0){
      result <- result + x[i]
    }
  }
  return(result)
}

result <- function_4(my_vector)
result

# Exercise 5 -----------------------------------------------------------------
# Create a function to find a minimun of a vector
my_vector <- sample(100, 5)
my_vector

minimun <- function(x){
  min <- x[1]
  for (i in 1:length(x)){
    if (x[i] < min){
      min <- x[i]
    }
  }
  return(min)
}

test <- minimun(my_vector)
test


# Exercise 6 -----------------------------------------------------------------
# Create a function to sort a vector
my_vector_1 <- sample(100, 5)
my_vector_2 <- sample(100, 5)

sorting <- function(vector_1, vector_2){
  x <- c(vector_1, vector_2)
  for (i in 1: (length(x) - 1)){
    for (j in 1: (length(x) - i))
      if (x[j] > x[j + 1]){
        temp <- x[j]
        x[j] <- x[j + 1]
        x[j + 1] <- temp
      }
  }
  return(x)
}

result <- sorting(my_vector_1, my_vector_2)
result


# Exercise 7 -----------------------------------------------------------------
# Write a code to perform the nut game.
# x <- sum(ceiling (6*runif (2))) # this is the function suggested from teacher
nut_1 <- sample(12, 1)
nut_2 <- sample(12, 1)
nut_1
nut_2

nut_game <- function(nut1, nut2){
  cat("Your numbers are:", nut1, "and", nut2, "\n")
  flag <- TRUE
  nut_sum1 <- nut1 + nut2
  cat("Sum at first attemp: ", nut_sum1, "\n")
  
  if (nut_sum1 == 7 || nut_sum1 == 11){
    cat("First attempt \n")
    cat("You win! You got ", nut1, "and", nut2, "\n")
  } else if (nut_sum1 == 2 || 
             nut_sum1 == 3 || 
             nut_sum1 == 12){
    cat("First attempt \n")
    cat("You lose! You got ", nut1, "and", nut2, "\n")
  } else {
    iteration <- 1
    while (flag == TRUE){
      nut_1 <- sample(12, 1)
      nut_2 <- sample(12, 1)
      nut_sum2 <- nut_1 + nut_2
      cat("At iteration", iteration,
          "your numbers are:", nut_1, "and", nut_2, "\n")
      iteration <- iteration + 1
      if (nut_sum2 == nut_sum1){
        cat("Second attempt \n")
        cat("Your numbers are:", nut_1, "and", nut_2, "\n")
        cat("You win! You got ", nut_1, "and", nut_2, "\n")
        flag <- FALSE
      }
      if (nut_1 == 7 || nut2 == 7){
        cat("Second attempt \n")
        cat("Your numbers are:", nut_1, "and", nut_2, "\n")
        cat("You lose! You got a 7! ", nut_1, "and", nut_2, "\n")
        flag <- FALSE
      }
    }
  }
}

game <- nut_game(sample(12, 1), sample(12, 1))


# Exercise 8 -----------------------------------------------------------------
# Room with switches
switches <- rep(-1, 100)

person_in_the_room <- 1

while (person_in_the_room <= length(switches)){
  for (i in 1: length(switches)){
    if (i %% person_in_the_room == 0)
      switches[i] <- switches[i] * -1 }
  person_in_the_room <- person_in_the_room + 1 }

for (i in 1: length(switches)){
  if (switches[i] == 1) print(i) 
}


# Number of People different from switch
switches <- rep(-1, 100)
person_in_the_room <- 1
max_person_in_the_room <- 53
while (person_in_the_room <= max_person_in_the_room){
  for (i in 1: length(switches)){
    if (i %% person_in_the_room == 0)
      switches[i] <- switches[i] * -1 }
  person_in_the_room <- person_in_the_room + 1 }


#for (i in 1: length(switches)){
#  if (switches[i] == 1) print(i) }



#### ===== Lecture 5 =========================================================
# funzione per cambiare lo stato di un interruttore
trasf_int<-function(vec){
  for (i in 1:length(vec))
    if(vec[i]==F) {vec[i]<-T} else {vec[i]<-F}
  return(vec)
}  
x<-c(rep(F, 10), rep(T,10))  
trasf_int(x)

# funzione che individua gli interrutori accesi
int_acc<-function(n){ # n = numero di interruttori = n. persone
  int<-rep(F,n)
  for(i in 1:n)
    int[seq(i,n,i)]<-trasf_int(int[seq(i,n,i)])
  ris<-which(int==TRUE)
  cat("Gli interruttori accesi sono:", ris, "\n")
  return(ris)
}

int_acc(100)

int_acc(53)


simula_partita_WW <- function() {
  vittoria <- c(7, 11)  # Eventi successo
  sconfitta <- c(2, 3, 12)  # Eventi insuccesso
  n <- 1 # n. lanci
  
  # Lancio iniziale dei dadi
  lancio_iniziale <- sum(ceiling(6 * runif(2)))
  
  if (lancio_iniziale %in% vittoria) {
    cat("Hai vinto con un risultato di", lancio_iniziale, "dopo ", n, "lanci", "\n")
    return("Vittoria!")
  } else if (lancio_iniziale %in% sconfitta) {
    cat("Hai perso con un risultato di", lancio_iniziale, "dopo ", n, "lanci", "\n")
    return("Sconfitta!")
  } else {
    cat("Hai ottenuto un risultato di", lancio_iniziale, "e continuerai a lanciare.\n")
    while (TRUE) {
      n <- n + 1
      nuovo_lancio <- sum(ceiling(6 * runif(2)))
      cat("Risultato nuovo lancio:", nuovo_lancio, "\n")
      if (nuovo_lancio == lancio_iniziale) {
        cat("Hai vinto con un risultato di", lancio_iniziale, "dopo ", n, "lanci", "\n")
        return("Vittoria!")
      } else if (nuovo_lancio == 7) {
        cat("Hai perso perchè hai ottenuto un sette", "dopo ", n, "lanci", "\n" )
        return("Sconfitta!")
      }
    }
  }
}

# Esegui una simulazione di partita WW
risultato_partita <- simula_partita_WW()


#--- Lecture 5 ---
rm(list = ls())

#### Input / Output -----------------------------------------------------------
# Convert to numeric
x <- "546"
x
as.numeric(x)
as.character(x)



# Convert a number to a string
# The function is format(x, digits = 2, nsmall = 1)
format(c(6.0, 13.1), digits = 2, nsmall = 1) # 2 digits, 1 decimal
format(c(6.0, 13.1), digits = 1, nsmall = 1)
as.numeric(format(c(6.0, 13.1), digits = 2, nsmall = 1))
format(c(6.0, 13.1), digits = 2, nsmall = 0)
#digits: the total number of digits to be printed
#nsmall: the minimum number of digits to the right of the decimal point

## Visualize concatenated strings
cat(paste(letters, 100 * 1:26), sep = "\n")
x <- paste(letters, 100 * 1:26)
mode(x)



### Input from files ----------------------------------------------------------
# scan() is a function that reads a file and creates a vector
? scan
scan(file = "05/scan_file_example.txt", 
     what = 0, 
     n = -1, 
     sep = "", 
     skip = 0, 
     quiet = FALSE)

cat("TITLE extra line", "2 3 5 7", "11 13 17", 
    file = "05/05 ex.data", 
    sep = "\n")
pp <- scan("05/05 ex.data", 
           skip = 1, 
           quiet = TRUE)
pp
scan("05/05 ex.data", skip = 1)
scan("05/05 ex.data", skip = 1, 
     nlines = 1) # only 1 line after the skipped one
scan("05/05 ex.data", what = list("","",""))
scan("05/05 ex.data", what = list("","",""), 
     flush = TRUE) # flush means that the last line is not read
pp <- matrix(scan("05 ex.data", skip = 1, 
                  quiet = TRUE))
pp
unlink("05/05 ex.data")


### Input from the keyboard ---------------------------------------------------
# readline() is a function that reads a line from the keyboard
?substr
question <- function(){
  answer <- readline("Do you like R? ")
  if (substr(answer, 1, 1) == "n" | substr(answer, 1, 1) == "N") {
    cat("Impossible, you're lying!\n")
  } else {
    cat("I knew it!\n")
  }
}

result <- question()

#source("05/05 Interruttori.R", echo = TRUE)


### Output to files -----------------------------------------------------------
# write.table() is a function that writes a table to a file
write(x, 
      file = "05/05 data", 
      ncolumns = if(is.character(x)) 1 else 5, 
      append = FALSE)

scan("05/05 data", what = list("","","","",""))
# We can also use cat() to write to a file
#cat(..., file = "", sep = " ", append = FALSE)

# Example with cat()
# Dati di esempio 
nome <- "Alice" 
eta <- 30 
citta <- "New York"

# Apriamo un file per scrittura 
file_conn <- file ("05/05 output.txt", "w")
# Utilizziamo cat per scrivere i dati nel file 
cat ("Nome:", nome, "\n", file = file_conn) 
cat ("Età:", eta, "\n", file = file_conn) 
cat ("Città:", citta, "\n", file = file_conn) 

# Chiudiamo il file 
close (file_conn)

# Verifica il contenuto del file 
cat (readLines ("05/05 output.txt"), sep = "\n")



### Plotting ------------------------------------------------------------------ 
# plot() is a function that plots a graph
x <- seq(0, 5, by = 0.01) 
ramo.sup <- 2 * sqrt (x) 
ramo.inf <- -2 * sqrt (x) 
y.max <- max (ramo.sup) 
y.min <- min (ramo.inf) 
# type = "n" means that no points are plotted
#?plot
plot (c(-2, 5), c(y.min, y.max), type = "n", xlab = "x", ylab = "y") 
lines (x, ramo.sup, type = "b", col=387) # plot the upper branch
lines (x, ramo.inf, type = 'b', col=647) # plot the lower branch
abline (v = -1, col='blue') 
#abline(h = 0, col='blue')
points (1, 0, cex = 2, pch = 19, col='red')
text (1, 0, "focus (1, 0)", pos = 4) 
text (-1, y.min, "direttrice x = -1", pos = 4) 
title("Parabola y^2 = 4××")


colours() # list of colours

rm(list = ls())

# Set the seed to have the same random numbers
set.seed(123)

# Genera dati casuali per ciascuna variabile
var1 <- round(rnorm(100, 24.5, 2.5),2) # Normal distribution, mean = 24.5, sd = 2.5
var2 <- round(rnorm(100, 175, 5),2)
var3 <- round(runif(100, 52, 89),2) # Uniform distribution, min = 52, max = 89
var4 <- rpois(100, lambda = 3) # Poisson distribution, lambda = 3 means that the mean is 3
var5 <- round(rnorm(100, mean = 90, sd = 3),0)
var6 <- round(rexp(100, rate = 0.1),0) # Exponential distribution, rate = 0.1 means that the mean is 10
var7 <- round(runif(100),0) # Uniform distribution, min = 0, max = 1
var7[var7==0]<-"F"  # Change 0 with F
var7[var7==1]<-"M"  # Change 1 with M
# Crea un dataframe con le variabili

dataset <- data.frame(eta = var1, 
                      altezza = var2, 
                      peso = var3, 
                      esami = var4, 
                      voto_diploma = var5, 
                      km = var6,
                      genere = var7)

# Visualizza le prime righe del dataset
head(dataset)
summary(dataset)

# Graph the variables heigth and weight
plot(dataset$altezza, dataset$peso, xlab = "Height", ylab = "Weight", 
     main = "Height vs Weight")

# Attach the dataset could be useful to avoid to write dataset$altezza and dataset$peso
attach(dataset)
plot(altezza, peso, xlab = "Height", ylab = "Weight", 
     main = "Height vs Weight")

# Using xlim and ylim we can change the range of the axes
plot(altezza, peso, xlab = "Height", ylab = "Weight", 
     main = "Height vs Weight", xlim = c(160, 190), ylim = c(50, 100))

# Color in pink the points with gender == F
points(altezza[genere == "F"], peso[genere == "F"], col = "pink")

# Color in blue the points with gender == M
points(altezza[genere == "M"], peso[genere == "M"], col = "blue")

# Line that sign the hight of 180cm
abline(h = 75, col = "red")

abline(v = 180, col = "green")

par(mfrow = c(1, 2)) # Divide the plot in 2 rows and 2 columns)
plot(altezza[genere == "F"], peso[genere == "F"])
plot(altezza[genere == "M"], peso[genere == "M"])





#### ===== Lecture 6 ==========================================================
#--- Lecture 6 ---
rm(list = ls())

### Functions ---------------------------------------------------------------
# Factorial function
n_fatt <- function (n) { 
  if (n == 0) 
    return (1)
  n_fatt <- prod(1:n) 
  return (n_fatt)
}

result <- n_fatt(5)
result

# Binomial coefficient
comb_r_n <- function (n, r) { 
  if (is.numeric(n) == FALSE || is.numeric(r) == FALSE) 
    break ("n and r must be numeric")
  n_ch_r <- n_fatt (n) / (n_fatt (r) * n_fatt (n-r)) 
  return (n_ch_r)
}

result <- comb_r_n(10, 3) # 5 balls in 2 different ways
result
result <- comb_r_n("f", 5) # 5 balls in 5 different ways


### Flussi basati su funzioni ------------------------------------------------
swap <- function (x){ 
  y <- x[2]
  x[2] <- x[1] 
  x[1] <- y 
  return (x)
}

x <- c(7, 8, 9) 
x[1:2] <- swap (x[1:2]) 
x[2:3] <- swap (x[2:3])
x


### Scope di una funzione ----------------------------------------------------
test2 <- function (x) { 
  #z <- 5 # z here is a local variable
  y <- x + z
  return (y)
}

# Prova
z <- 1 
test2 (1)
## [1] 2
z <- 2 
test2 (1)
## [1] 3


### Argomenti opzionali e valori di default ----------------------------------
test3 <- function (x = 1, y = 1, z = 1) { 
  return (x * 100 + y * 10 + z)
}
# Prova
test3(2, 2)  # z = 1
## [1] 221 
test3(y = 2, z = 2) # × = 1
## [1] 122


### Programmazione vettoriale basata su funzioni ----------------------------
## Example of sapply ---------------------------------------------------------
x <- 1:10
log(x)

sapply(x, log) # same result as above

somma_quadrato_10 <- function (x){ 
  risultato <- x^2 + 10 
  return (risultato)
}

# Vettore di numeri 
numeri <- c(1, 2, 3, 4, 5)
# funzione somma_quadrato_10 a ciascun numero 
risultati <- sapply(numeri, somma_quadrato_10)
# Risultati 
cat("Risultati:", risultati, "\n")
# Risultati: 11 14 19 26 35


## Example of apply ----------------------------------------------------------
# matrice di dati fittizia
matrice_dati <- matrix(rnorm(20), 5, 4)

# Calcolo di alcune statistiche su un vettore
calcola_statistiche <- function(vettore){
  media <- mean(vettore)
  deviazione_standard <- sd(vettore)
  massimo <- max(vettore)
  minimo <- min(vettore)
  return(c(Media = media, SD = deviazione_standard, 
           Massimo = massimo, Minimo = minimo))
}

# apply applica calcola_statistiche a ciascuna riga della matrice
# 1 indica che l'operazione deve essere applicata alle righe
# 2 indica che l'operazione deve essere applicata alle colonne
risultati <- apply(matrice_dati, 1, calcola_statistiche)

print(risultati)


## Example of mapply ---------------------------------------------------------
# funzione che calcola la somma dei quadrati di due numeri 
calcola_somma_quadrati <- function(x, y) {
  somma_quadrati <- x^2 + y^2
  return(somma_quadrati) 
}
# Definiamo due vettori 
vettore_x <- c(1, 2, 3, 4, 5) 
vettore_y <- c(6, 7, 8, 9, 10)
# mapply applica la calcola_somma_quadrati ai due vettori 
risultati <- mapply(calcola_somma_quadrati, vettore_x, vettore_y)
# Risultati 
print(risultati)
## [1] 37 53 73 97 125


### Programmazione ricorsiva -------------------------------------------------
nfatt2 <- function(n) {
  ris <- 0 # calcolo n fattoriale
  if (n == 1) {
    cat("uso nfatt2(1)\n") 
    ris<-1 
    return(ris)
  } else {
    cat("uso nfatt2(", n, ")\n", sep = "") 
    ris<-n*nfatt2(n-1) 
    return(ris)
  }
}

nfatt2(5)

### Debugging di funzioni ----------------------------------------------------
nfatt2 <- function(n) {
  ris<-0 
  browser() # Inseriamo una chiamata per avviare il debugging
  if (n == 1) {
    cat("uso nfatt2(1)\n") 
    ris<-1 
    return(ris)
  } else {
    cat("uso nfatt2(", n, ")\n", sep = "") 
    ris<-n*nfatt2(n-1) 
    return(ris)
  }
}

nfatt2(5)

### Exercises ----------------------------------------------------------------
# Exercise 1 ---------------------------------------------------------------
# The euclidean length of a vector is defined as the square root 
#of the sum of the squares of the vector elements.
l_euclid <- function(x){
  return(sqrt(sum(x^2)))
}

x <- c(1, 2, 3)
l_euclid(x)


# Exercise 2
# a ---------------------------------------------------------------
# We simulate the roll of a die.
x <- sample(1:6, 4, replace = TRUE)

# Teacher solution
if (6 %in% x) {
  cat("You rolled a 6!\n")
} else {
  cat("You did not roll a 6.\n")
}

# My solution
for (i in 1:4) {
  if (x[i] == 6) {
    cat("You win!\n")
    break
  } else {
    cat("You did not roll a 6.\n")
  }
}

# b ---------------------------------------------------------------
# Modify the previous code in order to win if you get a 6 in n rolls.
# The default value of n is 4.
win_lose <- function(x,n = 4){
  for (i in 1:n){
    flag <- 0
    if (x[i] == 6) {
      flag <- 1
      break
    }
  }
  if (flag == 1) {
    cat("You win!\n")
  } else {
    cat("You did not roll a 6.\n")
  }
}

x <- sample(1:6, 4, replace = TRUE)
n <- 4
win_lose(x, n)


# c ---------------------------------------------------------------
# Simulate N games of the previous game.
# Calculate the proportion of wins.

win_lose_counter <- function(x,n = 4){
  win_counter <- 0
  for (i in 1:n){
    if (x[i] == 6) {
      win_counter <- win_counter + 1
    }
  }
  return(win_counter)
}

# n = 4. N = 100, 1000, 10000
probabilities <- function(N){
  n <- 4
  win_counter_global <- 0
  for (i in 1:N){
    x <- sample(1:6, 4, replace = TRUE)
    win_counter_global <- win_counter_global + win_lose_counter(x, n)
  }
  cat("Total number of games:", N * n, "\n")
  cat("Number of wins:", win_counter_global, "\n")
  teorical_probability <- 1 - (5/6)^n
  cat("Teorical probability:", teorical_probability, "\n")
  simulation_result <- win_counter_global / (N * n)
  cat("Simulation result:", simulation_result, "\n")
  difference <- abs(teorical_probability - simulation_result)
  cat("Difference:", difference, "\n")
}

probabilities(100)
probabilities(1000)
probabilities(10000)

# d ---------------------------------------------------------------
# Save the results of the N simulations in file "results.txt".

win_lose_print <- function(x, n = 4) {
  result <- character(n)
  for (i in 1:n) {
    if (x[i] == 6) {
      result[i] <- "You win!"
    } else {
      result[i] <- "You lose!"
    }
  }
  return(result)
}

save_probabilities <- function(N, n = 4) {
  file_conn <- file("06/results.txt", "w")
  
  for (i in 1:N) {
    x <- sample(1:6, n, replace = TRUE)
    result <- win_lose_print(x, n)
    cat(result, file = file_conn, sep = "\n", append = TRUE)
  }
  
  close(file_conn)
}

save_probabilities(100)
save_probabilities(1000)
save_probabilities(10000)

cat (readLines ("06/results.txt"), sep = "\n")



# Exercise 3 ---------------------------------------------------------------
# Life game
set.seed(123)
## Teacher solution
A <- matrix(sample(c(0, 1), 100, replace = TRUE), 50, 50) 
A 

n_vicini <- function(A, i, j) {
  riga.sup <- ifelse(i > 1, i -1 , nrow(A))
  riga.inf <- ifelse(i < nrow(A), i + 1, 1)
  colonna.sx <- ifelse(j > 1, j - 1, ncol(A))
  colonna.dx <- ifelse(j < ncol(A), j + 1, 1)
  
  somma <- sum(A[c(riga.sup, i, riga.inf), c(colonna.sx, j, colonna.dx)])
  if (A[i, j] == 1){
    somma <- somma - 1
  }
  
  return(somma)
}

n_vicini(A, 4, 4)


# Exercise 4 ---------------------------------------------------------------
# Binomial coefficient
# Create a function that calculates the factorial of a number.
factorial <- function(n) {
  if (n == 0) {
    return(1)
  } else {
    return(n * factorial(n - 1))
  }
}

# Create a function that calculates the binomial coefficient 
#using the factorial function.
binomial <- function(n, k) {
  return(factorial(n) / (factorial(k) * factorial(n - k)))
}

# Create a function that calculates the binomial coefficient
# using the recursive definition of the binomial coefficient.
binomial_split <- function(n, k) {
  if (k == 0) {
    return(1)
  } else if (k == n) {
    return(1)
  } else {
    return(binomial_split(n - 1, k - 1) + binomial_split(n - 1, k))
  }
}

result1 <- binomial(5, 2)
result2 <- binomial_split(5, 2)
result1 == result2
result1
result2


rm(list = ls())

set.seed(42)
A<-matrix(sample(c(0,1), 100, replace=T),50,50)
A

#i=4
#j=3

#n_vicini<-function(A, i, j){
#  riga.sup<-ifelse(i>1,  i-1, nrow(A))
#  riga.inf<-ifelse(i<nrow(A),  i+1, 1)
#  colonna.sx <- ifelse(j>1,  j-1, ncol(A))
#  colonna.dx <- ifelse(j<ncol(A),  j+1, 1)
#  somma <- sum(c(A[riga.sup,][(i-1):(i+1)],
#                 A[i,colonna.sx] , 
#                 A[i,colonna.dx] ,
#                 A[riga.inf,][(i-1):(i+1)]))
#  c(riga.sup[(i-1):(i+1)], 
#    colonna.sx[i], colonna.dx[i], 
#    riga.inf[(i-1):(i+1)])
#return(somma)                 
#}

n_vicini <- function(A, i, j) {
  riga.sup <- ifelse(i > 1, i - 1, nrow(A))
  riga.inf <- ifelse(i < nrow(A), i + 1, 1)
  colonna.sx <- ifelse(j > 1, j - 1, ncol(A))
  colonna.dx <- ifelse(j < ncol(A), j + 1, 1)
  somma <- sum(c(
    A[riga.sup, colonna.sx], A[riga.sup, j], A[riga.sup, colonna.dx],
    A[i, colonna.sx],                 A[i, j],                 A[i, colonna.dx],
    A[riga.inf, colonna.sx], A[riga.inf, j], A[riga.inf, colonna.dx]
  ))
  return(somma)
}


gdv <- function(A) {
  deleted_positions <- list()  # List to store deleted positions
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      nv <- n_vicini(A, i, j)
      if (A[i, j] == 1 && nv < 2) {
        A[i, j] <- 0
        deleted_positions <- append(deleted_positions, list(c(i, j)))
      }
      if (A[i, j] == 1 && nv > 3) {
        A[i, j] <- 0
        deleted_positions <- append(deleted_positions, list(c(i, j)))
      }
      if (A[i, j] == 0 && (nv == 3)) A[i, j] <- 1
    }
  }
  # Plot deleted positions in red
  for (pos in deleted_positions) {
    points(pos[1], pos[2], col = "red")
  }
  return(A)
}

gioco_gdv<-function(A, n){
  plot(1:nrow(A), 1:ncol(A), type="n", 
       main=c("Generazione2",0), ylab="", xlab="")
  for(i in 1:nrow(A))
    for(j in 1:ncol(A))
      if(A[i,j]==1) points(i,j)
  for(b in 1:n){
    A<-gdv(A)
    plot(1:nrow(A), 1:ncol(A), type="n", 
         main=c("Generazione2",b), ylab="", xlab="")
    for(i in 1:nrow(A))
      for(j in 1:ncol(A))
        if(A[i,j]==1) points(i,j)
  }
  return(A)
}

A[,]<-0
A<-matrix(sample(c(0,1), 2500, replace=T),50,50) # where 100 instead of 2500
A
gioco_gdv(A, 5)

A[,]<-0
diag(A)<-1
A
gioco_gdv(A, 5)

A[,]<-0
for(i in 1:ncol(A)) A[i,ncol(A)-i+1]<-1
A[1,]<-A[,1]<-A[nrow(A),]<- A[,ncol(A)]<-1
A
gioco_gdv(A, 5)


rm(list = ls())
set.seed(42)
A<-matrix(sample(c(0,1), 100, replace=T),50,50)
A

n_vicini<-function(A, i, j){
  if(i %in% 2:(nrow(A)-1) & j %in% 2:(ncol(A)-1)){
    riga.sup<- A[i-1,(j-1):(j+1)]
    riga<-A[i,(j-1):(j+1)]
    riga.inf<- A[i+1,(j-1):(j+1)]
  } # ok
  if(i == nrow(A)& j %in% 2:(ncol(A)-1)){
    riga.sup<- A[i-1,(j-1):(j+1)]
    riga<-A[i,(j-1):(j+1)]
    riga.inf<- A[1,(j-1):(j+1)]
  } # ok
  if(i == 1& j %in% 2:(ncol(A)-1)){
    riga.sup<- A[nrow(A),(j-1):(j+1)]
    riga<-A[i,(j-1):(j+1)]
    riga.inf<- A[i+1,(j-1):(j+1)]
  } # ok
  if(i %in% 2:(nrow(A)-1) & j == 1){
    riga.sup<- A[i-1,c(ncol(A),j:(j+1))]
    riga<-A[i,(j-1):(j+1)]
    riga.inf<- A[i+1,c(ncol(A),j:(j+1))]
  } # ok
  if(i %in% 2:(nrow(A)-1) & j == ncol(A)){
    riga.sup<- A[i-1,c((j-1):j,1)]
    riga<-A[i,c((j-1):j,1)]
    riga.inf<- A[i+1,c((j-1):j,1)]
  } # ok
  # vertici
  if(i == 1 & j == 1){
    riga.sup<- A[nrow(A),c(ncol(A),j:(j+1))]
    riga<-A[i,c(ncol(A),j:(j+1))]
    riga.inf<- A[i+1,c(ncol(A),j:(j+1))]
  } # ok
  if(i == nrow(A) & j == 1){
    riga.sup<- A[i-1,c(ncol(A),j:(j+1))]
    riga<-A[i,c(ncol(A),j:(j+1))]
    riga.inf<- A[1,c(ncol(A),j:(j+1))]
  } # ok
  if(i == 1 & j == ncol(A)){
    riga.sup<- A[nrow(A),c((j-1):j,1)]
    riga<-A[i,c((j-1):j,1)]
    riga.inf<- A[i+1,c((j-1):j,1)]
  } # ok
  if(i == nrow(A) & j == ncol(A)){
    riga.sup<- A[i-1,c((j-1):j,1)]
    riga<- A[i,c((j-1):j,1)]
    riga.inf<- A[1,c((j-1):j,1)]
  } # ok
  somma<-sum(c(riga.sup,riga,riga.inf))
  if(riga[2]==1) somma<-somma-1
  return(somma)                 
}

gdv<-function(A){
  A2 <- A
  for(i in 1:nrow(A))
    for(j in 1:ncol(A)){
      nv<-n_vicini(A,i,j)
      if(A[i,j]==1 && nv < 2) A2[i,j]<-0 # replaced & with &&
      if(A[i,j]==1 && nv > 3) A2[i,j]<-0
      if(A[i,j]==0 && (nv == 3)) A2[i,j]<-1
    }
  return(A2)
}

gdv2<-function(A){
  for(i in 1:nrow(A))
    for(j in 1:ncol(A)){
      nv<-n_vicini(A,i,j)
      if(A[i,j]==1 && nv < 2){ # replaced & with &&
        A[i,j]<-0 
      } else if (A[i,j]==1 && nv > 3){ 
        A[i,j]<-0
      } else if (A[i,j]==0 && (nv == 3)) {
        A[i,j]<-1
      }
    }
  return(A)
}

gdv3 <- function(A) {
  deleted_positions <- list()
  for (i in 1:nrow(A)) {
    for (j in 1:ncol(A)) {
      nv <- n_vicini(A, i, j)
      if (A[i, j] == 1 && nv < 2) {
        A[i, j] <- 0
        deleted_positions <- append(deleted_positions, list(c(i, j)))
      }
      if (A[i, j] == 1 && nv > 3) {
        A[i, j] <- 0
        deleted_positions <- append(deleted_positions, list(c(i, j)))
      }
      if (A[i, j] == 0 && (nv == 3)) A[i, j] <- 1
    }
  }
  # Plot deleted positions in red
  for (pos in deleted_positions) {
    points(pos[1], pos[2], col = "red")
  }
  return(A)
}

gioco_gdv<-function(A, n){
  plot(1:nrow(A), 1:ncol(A), type="n", 
       main=c("Generazione",0), ylab="", xlab="")
  for(i in 1:nrow(A))
    for(j in 1:ncol(A))
      if(A[i,j]==1) points(i,j)
  for(b in 1:n){
    A<-gdv(A)
    plot(1:nrow(A), 1:ncol(A), type="n", 
         main=c("Generazione",b), ylab="", xlab="")
    for(i in 1:nrow(A))
      for(j in 1:ncol(A))
        if(A[i,j]==1) points(i,j)
  }
  return(A)
}

gioco_gdv2<-function(A, n){
  plot(1:nrow(A), 1:ncol(A), type="n", 
       main=c("Generazione",0), ylab="", xlab="")
  for(i in 1:nrow(A))
    for(j in 1:ncol(A))
      if(A[i,j]==1) points(i,j)
  for(b in 1:n){
    A<-gdv2(A)
    plot(1:nrow(A), 1:ncol(A), type="n", 
         main=c("Generazione",b), ylab="", xlab="")
    for(i in 1:nrow(A))
      for(j in 1:ncol(A))
        if(A[i,j]==1) points(i,j)
  }
  return(A)
}

A[,]<-0
A<-matrix(sample(c(0,1), 2500, replace=T),50,50) # where 100 instead of 2500
A
gioco_gdv(A, 5)
gioco_gdv2(A, 5)

A[,]<-0
diag(A)<-1
A
gioco_gdv(A, 5)

A[,]<-0
for(i in 1:ncol(A)) A[i,ncol(A)-i+1]<-1
A[1,]<-A[,1]<-A[nrow(A),]<- A[,ncol(A)]<-1
A
gioco_gdv(A, 5)



#### ===== Lecture 7 ==========================================================
#### --- Lecture 7 ---
rm(list = ls())

### Factor ====================================================================
capelli <- c("biondi", "neri", "marroni", "marroni", "neri",
             "grigi", "nessun_colore")
capelli
mode(capelli) # "character"
capelli <- factor(capelli)
levels(capelli) # by default are in alphabetical order
is.factor(capelli) # TRUE
capelli
mode(capelli) # "numeric"
#capelli[2] > capelli[1] # not meaningful for factors

# Ordered factor -> ordinal variable
reddito <- c("B", "A", "A", "B", "M", "M") 
reddito <- factor(reddito, levels = c("B", "M", "A"), ordered = TRUE) 
is.ordered(reddito) # TRUE
reddito
mode(reddito) # "numeric"
reddito[2] > reddito[1] # TRUE
reddito[4] > reddito[5] # FALSE




### Data  frame =================================================================
# Upload data ---------------------------------------------------------------
calciatori <- read.table("07/07calciatori.csv", header = T, sep = ";")
calciatori
mode(calciatori) # "list"

calciatori['Team'] # "list"
calciatori$Team # "character"
is.data.frame(calciatori) # TRUE

summary(calciatori)

is.factor(calciatori$Ruolo) # FALSE

# Set the type of the column "Ruolo" to factor
calciatori$Ruolo <- factor(calciatori$Ruolo)
is.factor(calciatori$Ruolo) # TRUE

# Set the type of the column "Ruolo" to ordered factor
calciatori$Ruolo <- factor(calciatori$Ruolo, 
                           levels = c("POR", "DF", "CENTR", "ATT"), 
                           ordered = TRUE)

# Set the type of the column "Team" to factor
calciatori$Team <- factor(calciatori$Team)
is.factor(calciatori$Team) # TRUE
is.ordered(calciatori$Team) # FALSE

summary(calciatori)

# Access to the data of a column ---------------------------------------------
x <- calciatori$PercPassagg
x[1:5] # First 5 elements of the column "PercPassagg"
calciatori[1:5,5] # Alternative way to get the same result

# Access multiple columns ----------------------------------------------------
minutiPassaggi <- calciatori[4:5]
minutiPassaggi[1:5, ]
is.data.frame(minutiPassaggi) # TRUE

mode(calciatori) # "list"
is.data.frame(calciatori) # TRUE

# Create a subset ------------------------------------------------------------
calciatori2 <- calciatori[1:100, c("Team", "minutiPP")]
calciatori2


# Get a summary -------------------------------------------------------------
# With summary we can see the type of each column
# and also discover if there are missing values
summary(calciatori)

calciatori[1:5, 3]
is.factor(calciatori$Ruolo) # TRUE, instead is False!!!
is.ordered(calciatori$Ruolo) # FALSE

# Order the factor -> ordinal variable
calciatori$Ruolo
# [331] "ATT"   "ATT"   "CENTR" "ATT"   "DF"    "POR"  
calciatori$Ruolo <- factor(calciatori$Ruolo, 
                           levels = c("POR", "DF", "CENTR", "ATT"), 
                           ordered = TRUE)
calciatori$Ruolo
# [323] ATT   ATT   CENTR ATT   DF    POR  
# Levels: POR < DF < CENTR < ATT

calciatori$Team

calciatori[4:5]
calciatori[c("minutiPP", "PercPassagg")]


minutiPassaggi <- calciatori[4:5] 
minutiPassaggi[1:5, ]
is.data.frame(minutiPassaggi) # TRUE

# Add a column ---------------------------------------------------------------
calciatori$TotMinuti <- calciatori$Partite * calciatori$minutiPP 
# Alternatives: calciatori[6] or calciatori["TotMinuti"] or 
#   calciatori[[6]] or calciatori[["TotMinuti"]]

mean(calciatori$TotMinuti)
summary(calciatori)

# Rename columns(variables) ---------------------------------------------------
names(calciatori)

# save a copy of the names
temp <- names(calciatori)

#(names.calciatori <- temp <- names(calciatori))
# Rename columns
names.calciatori <- c("P", "T", "R", "mPP", "PP", "TM") 
names.calciatori
names.calciatori <- temp
names.calciatori

# Size of the data frame ------------------------------------------------------
dim(calciatori)

# Rename rows ----------------------------------------------------------------
row.names(calciatori)
#! Don't do it if it is not necessary

# In theory it works, but with letters there are duplicates
#row.names(calciatori) <- sample(letters, nrow(calciatori), replace = TRUE)

# Something in the first part is wrong, check the file shared by the teacher
#row.names(calciatori[1:26]) <- sample(letters, nrow(calciatori), replace = TRUE)
#row.names(calciatori)

# Subset ---------------------------------------------------------------------
# There are another solution on top of the script
calciatori2 <- subset(calciatori, subset = Team %in% c("A", "G"), 
                      select = c(Ruolo, minutiPP, PercPassagg))
head(calciatori2)


# Save the data frame --------------------------------------------------------
write.table(calciatori2, 
            file = "07/07calciatori2.csv", 
            sep = ";", 
            row.names = FALSE)

# Check the file
read.table("07/07calciatori2.csv", header = T, sep = ";")




#### Lists ====================================================================
# Create a list --------------------------------------------------------------
my.list <- list("uno", TRUE, 3, c("f", "o", "u", "r"))
my.list[[4]] # [[]] is used to access the elements of a list
my.list[4] # [] is used to select a subset of a list
mode(my.list[4]) # "list"

x <- list(1, c(2, 3), c(4, 5, 6)) 
x
y <- list(10, c(20, 30), c(40, 50, 60))
y
z <- list(x, y)
z


# Rename the elements of a list ----------------------------------------------
my_list <- list(primo = "uno", 
                secondo = TRUE, 
                terzo = 3, 
                quarto = c("f", "o", "u", "r"))
names(my_list)

my_list$quarto

# Alternatively
names(my_list) <- c("Primo elemento", "Secondo elemento", "Terzo elemento", 
                    "Quarto elemento")
names(my_list)
my_list$`Quarto elemento`


# Unlist ---------------------------------------------------------------------
unlist(z) # Return a single vector
unlist(z, recursive = FALSE) # Returns the two lists in a single list

length(z) # 2
length(x) # 3
length(y) # 3
length(unlist(z)) # 12
length(unlist(z, recursive = FALSE)) # 6



# Upload a list --------------------------------------------------------------
albo <- readLines("07/07AlboDoro.txt")
albo
mode(albo) # "character"
length(albo) #16

squadre <- list()

for (i in albo) {
  divido <- unlist(strsplit(i, ", ")) # Split the string
  Nome_squadra <- divido[1] # Get the name of the team
  Anni <- as.numeric(divido[-1]) # Get the years
  squadre[[Nome_squadra]] <- Anni # Add the team to the list
}

print(squadre[1:2])


# tapply function ------------------------------------------------------------
# Example: mean of the minutes played per role
# The first argument is the variable to be analyzed
# The second argument is the factor
# la media dei minuti giocati a partita per ruolo
tapply(calciatori$minutiPP, calciatori$Ruolo, mean) 

# Index argument could be a list of factors
c_average <- tapply(calciatori$minutiPP, calciatori[c("Ruolo", "Team")], mean) 
str(c_average) # str() is used to see the structure of an object
c_average[1:4,1:5]


# lapply function ------------------------------------------------------------
# Returns a list
lapply(calciatori[4:6], mean)
# Output:

#$minutiPP
#[1] 37.35417

#$PercPassagg
#[1] 74.2256

#$TotMinuti
#[1] 2501.618

# Calculate the trimmed mean
lapply(calciatori[4:6], mean, trim = 0.1)


squadre
Post1986 <- function(x) x[x >= 1986] 
V.1986 <- lapply(squadre, Post1986) 
str(V.1986)

# sapply function ------------------------------------------------------------
# Returns a vector
sapply(calciatori[4:6], mean)

# Output: same as in line 169
#minutiPP PercPassagg   TotMinuti 
#37.35417    74.22560  2501.61815 

squadre

V.1970 <- function(x) return(1970 %in% x) 
names(squadre)[sapply(squadre, V.1970)]

# Sort
sort(sapply(squadre, length))



#### ===== Lecture 8 ==========================================================
#### --- Lecture 8 ---
rm(list = ls())

### Rappresentazioni grafiche =================================================
ufc <- read.csv("08/ufc.csv") 
str(ufc)
head(ufc)


### Funzione plot ==============================================================
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

plot(ufc$dbh.cm, ufc$tree, 
     xlab = "Diametro (cm)", ylab = "Specie")

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     pch = 19)

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     type = "h")

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     xlim = c(0, 60), ylim = c(0, 20))

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     lwd = 2)

plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     col="blue")

colours() # 657 colors
?plot # par option


### Funzione par ===============================================================
?par
# Set parameters to be used to the following plot
par(pch = 12, col = 7)
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

par(col=2, pch =3) 
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

par(col=2, pch =3, mfrow = c(2,3)) 
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")
par(pch = 25)
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

#quartz()
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)",
     col= "blue", 
     pch = 19, 
     main = "Alberi UFC")

# Get the value of a parameters
par()

# Get the value of a specific parameter
par("col")

par(col=2, pch =3) 
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")
par(col=1, pch = 1)

# Create a matrix space for plots
par(mfrow = c(1,1))
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")
par(col=1, pch = 1)

# Insert space around each graph
par(mar = c(1,1,1,1))
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")
par(col=1, pch = 1)

# Create space around the matrix of plots
par(oma = c(1,1,1,1))
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")
par(col=1, pch = 1)

# Create space around the matrix of plots
par(las = 1)
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")

# Squared plot
par(pty = "s")
plot(ufc$dbh.cm, ufc$height.m, 
     xlab = "Diametro (cm)", ylab = "Altezza (m)")


### Costruzione di un grafico step-by-step =====================================
par(mfrow = c(1,1))
opar1 <- par(las = 1, # 0 = parallel to axis, 1 = horizontal, 2 = perpendicular to axis
             mar=c(4,4,3,2)) 
plot(ufc$dbh.cm, ufc$height.m, 
     axes=FALSE, 
     xlab="", ylab="", 
     type="n")

points(ufc$dbh.cm, ufc$height.m,
       col = ifelse(ufc$height.m > 4.9, "blue", "red"), 
       pch = ifelse(ufc$height.m > 4.9, 1, 3))

axis(1) 
axis(2)

opar2 <- par(las=0) 
mtext("Diameter (cm)", 
      side=1, # 1 = bottom, 2 = left, 3 = top, 4 = right
      line=3) # distance from the axis
mtext("Height (m)", 
      side=2, 
      line=3)

box()

?legend
legend(x = 60, y = 15, # custom top left corner
       c("Albero normale", "Albero anomalo"), 
       col=c("blue", "red"), 
       pch=c(1, 3),
       bty="n") # bty = box type

legend("topleft", 
       c("Albero normale", "Albero anomalo"), 
       col=c("blue", "red"), 
       pch=c(1, 3))


### Salvataggio di un oggetto grafico ==========================================
pdf(file = "08/grafico.pdf", 
    width = 6, height = 5) 
plot(ufc$dbh.cm, ufc$height.m, 
     main = "Alberi UFC",
     xlab = "Dbh (cm)", ylab = "Altezza (m)")
dev.off() 

png(file = "08/grafico.png", 
    width = 1000, height = 800)
plot(ufc$dbh.cm, ufc$height.m, 
     main = "Alberi UFC",
     xlab = "Dbh (cm)", ylab = "Altezza (m)")
dev.off()


### Grafici Trellis ===========================================================
library(lattice)

# Conditional by one variable
histogram(~ dbh.cm | species, data = ufc) # ~ means that dbh depends from somthing

# Conditional two variables depending from one
histogram(~ dbh.cm + height.m | species, data = ufc)

# Conditional by two variables
histogram(~ dbh.cm | species + tree, data = ufc)


xyplot(height.m ~ dbh.cm | species, data = ufc)

summary(ufc)

? lattice
histogram(~ dbh.cm | species + tree, 
          group = tree, # group by tree
          data = ufc)

# Save the plot into an object
mio.grafico <- histogram(~ dbh.cm | species, data = ufc)
mio.grafico

# Split the plot
# c(1,2,3,4) 1 = col object, 2 = row object, 3 = col plot, 4 = row plot 
print(mio.grafico, split = c(2,1,2,3), more = TRUE)
print(mio.grafico, split = c(2,2,2,3), more = TRUE)
print(mio.grafico, split = c(1,2,2,3), more = TRUE)
print(mio.grafico, split = c(1,1,3,2), more = TRUE) # smaller plot
print(mio.grafico, split = c(3,3,5,5), more = TRUE) # even smaller plot


### Grafici in 3-D ============================================================
graphics.ufc <- read.csv("08/ufc-plots.csv") 
str(graphics.ufc)
head(graphics.ufc)

contourplot(vol.m3.ha ~ east * north, data = graphics.ufc,
            main = "Volume (m^3/ha)", 
            xlab = "Est (m)", 
            ylab = "Nord (m)", 
            region = TRUE, 
            aspect = "iso", # modify the aspect ratio
            col.regions = gray((11:1)/11))


wireframe(vol.m3.ha ~ east * north, data = graphics.ufc,
          main = "Volume (m^3/ha)", 
          xlab = "East (m)", ylab = "North (m)")
