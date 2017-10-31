library("parallel")
cluster <- makeCluster(detectCores()- 1)
binario <- function(d, l) {
  b <-  rep(FALSE, l)
  while (l > 0 | d > 0) {
    b[l] <- (d %% 2 == 1)
    l <- l - 1
    d <- bitwShiftR(d, 1)
  }
  return(b)
}

decimal <- function(bits, l) {
  valor <- 0
  for (pos in 1:l) {
    valor <- valor + 2^(l - pos) * bits[pos]
  }
  return(valor)
}

pruebas <- function(i){
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,] # fila 1 contiene el cero, etc.
  correcto <- decimal(binario(d, n), n)
  salida <- rep(FALSE, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    salida[i] <- resultado
  }
  r <- min(decimal(salida, n), k) # todos los no-existentes van al final
  return(r==correcto)
}

modelos <- read.csv("digitos.modelo", sep=" ", header=FALSE, stringsAsFactors=F)
porcentaje <- c()
#for(p in c(.01, .008, .005, .002, .001, .0009)){
modelos[modelos=='n'] <- 0.99 
modelos[modelos=='g'] <- 0.99 
modelos[modelos=='b'] <- 0.008 


r <- 5
c <- 3
dim <- r * c

tasa <- 0.15
tranqui <- 0.99
iter <- 10000
tope <- 9
digitos <- 0:tope
k <- length(digitos)

n <- floor(log(k-1, 2)) + 1
neuronas <- matrix(runif(n * dim), nrow=n, ncol=dim) # perceptrones

for (t in 1:5000) { # entrenamiento
  d <- sample(0:tope, 1)
  pixeles <- runif(dim) < modelos[d + 1,]
  correcto <- binario(d, n)
  for (i in 1:n) {
    w <- neuronas[i,]
    deseada <- correcto[i]
    resultado <- sum(w * pixeles) >= 0
    if (deseada != resultado) {
      ajuste <- tasa * (deseada - resultado)
      tasa <- tranqui * tasa
      neuronas[i,] <- w + ajuste * pixeles
    }
  }
}


clusterExport(cluster, c("pruebas", "neuronas", "binario", "decimal", "modelos", "tope" ,"k", "dim", "n"))
contadores <- parSapply(cluster, 1:iter, pruebas)

porcentaje <- c(porcentaje, (sum(contadores)/iter))
#}
stopCluster(cluster)
print(porcentaje)
#plot(x=negros, y=porcentaje.n, ylab= "Porcentaje de acierto", xlab= "Probabilidad negro")
#lines(x=negros, y=porcentaje.n)
#plot(x=grises, y=porcentaje.g, ylab= "Porcentaje de acierto", xlab= "Probabilidad grises")
#lines(x=grises, y=porcentaje.g)
#plot(x=blancos, y=porcentaje.b, ylab= "Porcentaje de acierto", xlab= "Probabilidad blancos")
#lines(x=blancos, y=porcentaje.b)
#plot(x = 1:8, y= porcentaje.c, xlab = "Combinación", ylab= "Porcentaje de acierto")
#lines(x = 1:8, y= porcentaje.c)