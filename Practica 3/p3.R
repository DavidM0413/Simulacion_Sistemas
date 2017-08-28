primo <- function(n) {
  if (n == 1 || n == 2) {
    return(TRUE)
  }
  if (n %% 2 == 0) {
    return(FALSE)
  }
  for (i in seq(3, max(3, ceiling(sqrt(n))), 2)) {
    if ((n > i) && (n %% i) == 0) {
      return(FALSE)
    }
  }
  return(TRUE)
}

d <- data.frame()
desde <- 10
hasta <-  300
original <- desde:hasta
invertido <- hasta:desde
aux <- c()
for(j in original){
  aux <- c(aux, primo(j))
}
primos <- c(original[aux], original[as.logical(-aux + 1)])
noprimos <- c(original[as.logical(-aux + 1)], original[aux])
replicas <- 10
suppressMessages(library(doParallel))

nucleos<-as.numeric(detectCores())
for(i in 1:nucleos){
registerDoParallel(makeCluster(i))
ot <-  numeric()
it <-  numeric()
at <-  numeric()
pt <- numeric()
npt <- numeric()
for (r in 1:replicas) {
  ot <- c(ot, system.time(foreach(n = original, .combine=c) %dopar% primo(n))[3]) # de menor a mayor
  it <- c(it, system.time(foreach(n = invertido, .combine=c) %dopar% primo(n))[3]) # de mayor a menor
  at <- c(at, system.time(foreach(n = sample(original), .combine=c) %dopar% primo(n))[3]) # orden aleatorio
  pt <- c(pt, system.time(foreach(n = primos, .combine=c) %dopar% primo(n))[3])
  npt <- c(npt, system.time(foreach(n = noprimos, .combine=c) %dopar% primo(n))[3])
  
}

d <- rbind(d, as.numeric(ot), as.numeric(it), as.numeric(at), as.numeric(pt), as.numeric(npt))
stopImplicitCluster()
}
means <- c()
for(a in 1:(5*as.numeric(detectCores()))){
  means <- c(means, mean(as.numeric(d[a,])))
}
matriz <- matrix(means, ncol = 5)
colnames(matriz) <- c("ot", "it", "at", "pt", "npt")
rownames(matriz) <- c("1 núcleo", "2 núcleos", "3 núcleos", "4 núcleos", "5 núcleos", "6 núcleos", "7 núcleos", "8 núcleos")

#d <- cbind(d,means)
#summary(ot)
#summary(it)
#summary(at)

n<-c()
for( i in 1:40){ 

  n<-c(n, as.numeric(d[i,]))
}
ordenes <- c(rep(1, 10), rep(2, 10), rep(3, 10), rep(4, 10), rep(5, 10))
nucleos <- c(rep(1,50), rep(2,50), rep(3,50), rep(4,50), rep(5,50), rep(6,50), rep(7,50), rep(8,50))
respuesta <- data.frame()
respuesta <- cbind(n, ordenes, nucleos)

kruskal.test(respuesta[,1] ~ respuesta[,2])
kruskal.test(respuesta[,1] ~ respuesta[,3])
barplot(t(matriz), beside = T, col = 1:5, legend.text = c("ot", "it", "at", "pt", "npt"))
