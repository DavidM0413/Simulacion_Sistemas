actualizar <- function(){
  a <- agentes[i, ]
  if (contagios[i] & a$estado == "S") {
    a$estado <- "I"
  } else if (a$estado == "I") { # ya estaba infectado
    if (runif(1) < pr) {
      a$estado <- "R" # recupera
    }
  }
  a$x <- a$x + a$dx
  a$y <- a$y + a$dy
  if (a$x > l) {
    a$x <- a$x - l
  }
  if (a$y > l) {
    a$y <- a$y - l
  }
  if (a$x < 0) {
    a$x <- a$x + l
  }
  if (a$y < 0) {
    a$y <- a$y + l
  }
  return(as.vector(a))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))

porcentaje <- c()
resultado <- data.frame()
for(b in seq(0.05, .95, by=.05)){
  for(c in 1:10){
l <- 1.5
n <- 50
pi <- b
pr <- 0.02
#pv <- b
v <- l / 30
agentes <- data.frame(x = runif(n, 0, l), y = runif(n, 0, l), dx = runif(n, -v, v), dy = runif(n, -v, v), estado = sample(c("S", "I"), n, replace=TRUE, c(1-pi, pi)))
agentes$estado = as.factor(agentes$estado)
levels(agentes$estado) = c(levels(agentes$estado), "R")
epidemia <- integer()
r <- 0.1
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
for (tiempo in 1:tmax) {
  infectados <- dim(agentes[agentes$estado == "I",])[1]
  epidemia <- c(epidemia, infectados)
  if (infectados == 0) {
    break
  }
  contagios <- rep(FALSE, n)
  for (i in 1:n) { # posibles contagios
    a1 <- agentes[i, ]
    if (a1$estado == "I") { # desde los infectados
      for (j in 1:n) {
        if (!contagios[j]) { # aun sin contagio
          a2 <- agentes[j, ]
          if (a2$estado == "S") { # hacia los susceptibles
            dx <- a1$x - a2$x
            dy <- a1$y - a2$y
            d <- sqrt(dx^2 + dy^2)
            if (d < r) { # umbral
              p <- (r - d) / r
              if (runif(1) < p) {
                contagios[j] <- TRUE
              }
            }
          }
        }
      }
    }
  }
  aux <- data.frame()
  aux <- rbind(yolo,foreach(i = 1:n, .combine=c) %dopar% actualizar())
  aux1 <- data.frame()
  colnames(aux)<-rep(seq(1:5),50)
  for(y in 1:50){
    v<-(aux[,(5*y-4):(5*y)])
    v1<-c()
    aux1<-rbind(aux1,v)
  }
  colnames(aux1)<-c("x","y","dx","dy","estado")
  agentes <- aux1
  aS <- agentes[agentes$estado == "S",]
  aI <- agentes[agentes$estado == "I",]
  aR <- agentes[agentes$estado == "R",]
  tl <- paste(tiempo, "", sep="")
}
porcentaje <- c(porcentaje, (max(epidemia)/n))
plot(1:length(epidemia), 100 * epidemia / n, xlab="Tiempo", ylab="Porcentaje de infectados")
  }
  print(porcentaje)
  resultado <- rbind(resultado, porcentaje)
  porcentaje <- c()
}
stopImplicitCluster()
