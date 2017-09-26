library("lattice")
library("latticeExtra")
g <- function(x, y) {
  return(((x + 0.5)^4 - 30 * x^2 - 20 * x + (y + 0.5)^4 - 30 * y^2 - 20 * y)/100)
}

low <- -4
high <- 4
step <- 0.5
replicas <- 100

replica <- function(r, t, e) {
  x <- runif(2, low, high)
  respuesta <- x
  for (tiempo in 1:r) {
    delta <- runif(2, -step, step)
    x.delta <- x + delta
    if (x.delta[1] > 4) {
      x.delta[1] <- x.delta[1] - 8
    } else{
      x.delta[1] <- x.delta[1]
    }
    if (x.delta[2] > 4) {
      x.delta[2] <- x.delta[2] - 8
    } else{
      x.delta[2] <- x.delta[2]
    }
    if (x.delta[1] < -4) {
      x.delta[1] <- x.delta[1] + 8
    }else{
      x.delta[1] <- x.delta[1]
    }
    if (x.delta[2] < -4) {
      x.delta[2] <- x.delta[2] + 8
    } else{
      x.delta[2] <- x.delta[2]
    }
    
    
    if (g(x.delta[1], x.delta[2]) > g(x[1], x[2])) {
      x <- x.delta
      respuesta <- c(respuesta, x)
    } else {
      d <- g(x.delta[1], x.delta[2]) - g(x[1], x[2])
      if(runif(1) < exp(d/t)){
        x <- x.delta
        t <- t*e
        respuesta <- c(respuesta, x)
      }
    }
  }
  return(respuesta)
}



respuesta <- c()
for (t in seq(1, 12, by = 2)) {
  for(e in seq(.3, .7, by= .05)){
    resultados <- replica(10000, t, e)
    for(i in seq(1, (length(resultados) -1), by= 2)){
      respuesta <- c(respuesta, g(resultados[i], resultados[i + 1]))
    }
    png(paste("p7_","T= ", t, ", e= ", e, ".png", sep=""))
    plot(respuesta, ylab= "g(x,y)", main = paste("T= ", t, ", e= ", e), xlab = "")
    lines(respuesta, type = "l")
    abline(h=0.0666822, col= "red")
    graphics.off()
    respuesta <- c()}
}


