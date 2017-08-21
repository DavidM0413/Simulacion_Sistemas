library(parallel)
iteraciones <- c()
respuesta <- data.frame()
vector <- seq(0,1, by = .05)
cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")

paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  return(1 * ((sum(vecindad) - actual[fila, columna]) == 3))
}

for(b in vector){
  for (a in 1:20){
    library(parallel)
    dim <- 10
    num <-  dim^2

actual <- matrix(1*(runif(num)> b), nrow=dim, ncol=dim)
suppressMessages(library("sna"))

for (iteracion in 1:15) {
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  if (sum(siguiente) == 0) { # todos murieron
    
    break;
  }
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  
}
iteraciones[a] <- iteracion

}
 respuesta <- rbind(respuesta, iteraciones) 
}
stopCluster(cluster)
rownames(respuesta) <- vector
boxplot(t(respuesta), xlab = "Probabilidades")
