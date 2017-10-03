library(testit) # para pruebas, recuerda instalar antes de usar
#creacion (inicio)
rotura <- function(x) {
  return (1 / (1 + exp((c - x) / d)))
}
union <- function(x) {
  return (exp(-x / c))
}
romperse <- function(tam, cuantos) {
  romper <- round(rotura(tam) * cuantos) # independientes
  resultado <- rep(tam, cuantos - romper) # los demas
  if (romper > 0) {
    for (cumulo in 1:romper) { # agregar las rotas
      t <- 1
      if (tam > 2) { # sample no jala con un solo valor
        t <- sample(1:(tam-1), 1)
      }
      resultado <- c(resultado, t, tam - t)
    }
  }
  assert(sum(resultado) == tam * cuantos) # no hubo perdidas
  return(resultado)
}

unirse <- function(tam, cuantos) {
  unir <- round(union(tam) * cuantos) # independientes
  if (unir > 0) {
    division <- c(rep(-tam, unir), rep(tam, cuantos - unir))
    assert(sum(abs(division)) == tam * cuantos)
    return(division)
  } else {
    return(rep(tam, cuantos))
  }
}
rompimiento <- function(i){
  return(romperse(as.numeric(freq[i,][1]), as.numeric(freq[i,][2])))
}
uniones <- function(i){
  return(unirse(as.numeric(freq[i,][1]), as.numeric(freq[i,][2])))
}
library(parallel)
cluster <- makeCluster(detectCores()- 1)
clusterExport(cluster, c("romperse", "rotura", "rompimiento", "assert", "uniones", "union", "unirse"))
respuesta <- data.frame()
for(r in 1:10){
tiempos <- c()
for(pot in seq(5, 25, by= 5)){
  ti <- Sys.time()
  k <- pot*1000
  n <- 30*k
  
originales <- rnorm(k)
cumulos <- originales - min(originales) + 1
cumulos <- round(n * cumulos / sum(cumulos))
assert(min(cumulos) > 0)
diferencia <- n - sum(cumulos)
if (diferencia > 0) {
  for (i in 1:diferencia) {
    p <- sample(1:k, 1)
    cumulos[p] <- cumulos[p] + 1
  }
} else if (diferencia < 0) {
  for (i in 1:-diferencia) {
    p <- sample(1:k, 1)
    if (cumulos[p] > 1) {
      cumulos[p] <- cumulos[p] - 1
    }
  }
}
#creacion (fin)

#ajustes (incio)
assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
assert(sum(cumulos) == n)
#ajustes (fin)

#parametros (inicio)
c <- median(cumulos) # tamanio critico de cumulos
d <- sd(cumulos) / 4 # factor arbitrario para suavizar la curva
#parametros (fin)
clusterExport(cluster, c("c", "d"))
#funciones (incio)

#funciones (inicio)
freq <- as.data.frame(table(cumulos))
names(freq) <- c("tam", "num")
freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
duracion <- 30
digitos <- floor(log(duracion, 10)) + 1
#funciones ()



#aqui empieza
for (paso in 1:duracion) {
  assert(sum(cumulos) == n)
  cumulos <- integer()
  clusterExport(cluster, "freq")
  cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), rompimiento))
  a <- c()
  for(i in 1:length(cumulos)){
    a <-c(a, cumulos[[i]])
  }
  cumulos <- a
  
  assert(sum(cumulos) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  freq <- as.data.frame(table(cumulos)) # actualizar urnas
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  cumulos <- integer()
  
  clusterExport(cluster, "freq")
  cumulos <- as.vector(parSapply(cluster, 1:(dim(freq)[1]), uniones))
  a <- c()
  for(i in 1:length(cumulos)){
    a <-c(a, cumulos[[i]])
  }
  cumulos <- a
  
  assert(sum(abs(cumulos)) == n)
  assert(length(cumulos[cumulos == 0]) == 0) # que no haya vacios
  juntarse <- -cumulos[cumulos < 0]
  cumulos <- cumulos[cumulos > 0]
  assert(sum(cumulos) + sum(juntarse) == n)
  nt <- length(juntarse)
  if (nt > 0) {
    if (nt > 1) {
      juntarse <- sample(juntarse)
      for (i in 1:floor(nt / 2) ) {
        cumulos <- c(cumulos, juntarse[2*i-1] + juntarse[2*i])
      }
    }
    if (nt %% 2 == 1) {
      cumulos <- c(cumulos, juntarse[nt])
    }
  }
  assert(sum(cumulos) == n)
  freq <- as.data.frame(table(cumulos))
  names(freq) <- c("tam", "num")
  freq$tam <- as.numeric(levels(freq$tam))[freq$tam]
  assert(sum(freq$num * freq$tam) == n)
  tl <- paste(paso, "", sep="")
  
}
tf <- Sys.time()
t_t <- tf - ti
tiempos <- c(tiempos, t_t)
print(paste(t_t, r))
}
respuesta <- rbind(respuesta, as.vector(tiempos))
}
stopCluster(cluster)
colnames(respuesta) <- c(5000, 10000, 15000, 20000, 25000)
boxplot(respuesta, xlab= "Cantidad de Cúmulos", ylab= "Tiempo", main = "Paralelo")
