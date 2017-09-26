library("lattice")
library("latticeExtra")
g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -4
high <- 4
step <- 0.5
replicas <- 100

replica <- function(t) {
  curr <- runif(2, low, high)
  best <- curr
 
  for (tiempo in 1:t) {
    best1 <- numeric()
    best2 <- numeric()
    delta.x <- runif(1, 0, step)
    delta.y <- runif(1, 0, step)
    x1 <- curr[1] - delta.x
    x2 <- curr[1] + delta.x
    y1 <- curr[2] - delta.y
    y2 <- curr[2] + delta.y
    if (x2 > 4) {
      x2 <- x2 - 8
    } else{
      x2 <- x2
    }
    if (y2 > 4) {
      y2 <- y2 - 8
    } else{
      y2 <- y2
    }
    if (x1 < -4) {
      x1 <- x1 + 8
    }else{
      x1 <- x1
    }
    if (y1 < -4) {
      y1 <- y1 + 8
    } else{
      y1 <- y1
    }
    
    if (g(x1, curr[2]) < g(x2, curr[2])) {
      best1 <- x2
    } else {
      best1 <- x1
    }
    
    if (g(curr[1], y2) < g(curr[1], y1)) {
      best2 <- y1
    } else {
      best2 <- y2
    }
    
    if (g(best1, curr[2]) < g(curr[1], best2)) {
      curr <- c(curr[1], best2)
    } else {
      curr <- c(best1, curr[2])
    }
    if (g(curr[1], curr[2]) > g(best[1], best[2])) {
      best <- curr
    }
  }
  return(best)
}

suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

resultados <- c()
for (pot in 2:4) {
  tmax <- 10^pot
  resultados <- c(resultados, foreach(i = 1:replicas, .combine=c) %dopar% replica(tmax))
}
stopImplicitCluster()
respuesta <- c()
resultado <- data.frame()
for(i in seq(1, (length(resultados) -1), by= 2)){
  respuesta <- c(respuesta, g(resultados[i], resultados[i + 1]))
}
aux <- matrix(resultados, ncol = 2, byrow = T)
resultado <- cbind(respuesta, aux, c(rep(100, 100), rep(1000, 100), rep(10000, 100)))
print(resultado[max(resultado[,1])== resultado[,1],])