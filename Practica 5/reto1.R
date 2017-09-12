circulo <- function(){
  x <- runif(cuantos, min = -.5, max = .5)
  y <- runif(cuantos, min = -.5, max = .5)
  in.circle <- x^2 + y^2 <= 0.5^2
  mc.pi <- (sum(in.circle)/cuantos)*4
  return(mc.pi)
}
e_pi <- c()
tiempos <- c()
e_circulo <- c()
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores()-1))
for(i in 1:5){
cuantos <- 500000*i^2
e_circulo <- c(e_circulo, foreach(i = 1:10, .combine=c) %dopar% circulo())
tiempos <- c(tiempos, as.numeric(system.time(foreach(i = 1:10, .combine=c) %dopar% circulo())[3]))
}
stopImplicitCluster()
promedios <- c()
m_pi <- matrix(e_circulo, nrow = 5, byrow = T)
delta <- e_circulo - pi
m_delta <- matrix(delta, nrow = 5, byrow = T)
boxplot(t(m_delta))
plot(tiempos)
lines(tiempos)
plot(delta)
abline(h=0, col= "red")