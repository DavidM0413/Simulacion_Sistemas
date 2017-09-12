valor <- data.frame()
tiempo <- data.frame()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
f <- function(x) { return(1 / (exp(x) + exp(-x))) }
g <- function(x) { return((2 / pi) * f(x)) }
suppressMessages(library(distr))
generador  <- r(AbscontDistribution(d = g))
desde <- 3
hasta <- 7
inicio <- -6
final <- -inicio
paso <- 0.25
x <- seq(inicio, final, paso)
cuantos <- 500
parte <- function() {
  valores <- generador(pedazo)
  return(sum(valores >= desde & valores <= hasta))
}
valores <- c()
tiempos <- c()

for(i in 1:5){
  for(j in 1:10){


pedazo <- i*50000

montecarlo <- foreach(i = 1:cuantos, .combine=c) %dopar% parte()
tiempos <- c(tiempos, as.numeric(system.time(foreach(i = 1:cuantos, .combine=c) %dopar% parte())[3]))
integral <- sum(montecarlo) / (cuantos * pedazo)
valores <- c(valores, (pi / 2) * integral)
}
 
}
stopImplicitCluster()
promedios <- c()
m_tiempos <- matrix(tiempos, nrow = 5, byrow = T)
delta <- valores - 0.048834
m_delta <- matrix(delta, nrow = 5, byrow = T)
for(p in 1:5){
  shapiro.test(m_tiempos[p,])
  plot(m_delta[p,])
  abline(h= 0, col = "red")
  promedios <- c(promedios, mean(m_tiempos[p,]))
}
plot(promedios)
lines(promedios)

