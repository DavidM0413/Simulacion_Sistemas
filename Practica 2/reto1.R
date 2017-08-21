library(parallel)
suppressMessages(library("sna"))
dim <- 20
num <-  dim^2
nucleos <- 8
pos.nucleos <- ceiling(num*runif(nucleos))
prob.nucleos <- runif(nucleos)
ve <- rep(0, num)
aux <- c()
for(i in 1:nucleos){
  ve[pos.nucleos[i]] <- prob.nucleos[i]
}

actual <- matrix(ve, nrow=dim, ncol=dim)
png("p2_t0.png")
plot.sociomatrix(actual, diaglab=FALSE, main="Inicio")
paso <- function(pos) {
  fila <- floor((pos - 1) / dim) + 1
  columna <- ((pos - 1) %% dim) + 1
  vecindad <-  actual[max(fila - 1, 1) : min(fila + 1, dim),
                      max(columna - 1, 1): min(columna + 1, dim)]
  if (actual[fila,columna] == 0){
    return(max(vecindad))
  }
  
  else{
    return(actual[fila,columna])
  }
  }

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "dim")
clusterExport(cluster, "paso")
clusterExport(cluster, "ve")
clusterExport(cluster, "semillas")
for (iteracion in 1:20) {
  if (all(actual !=0)) { # todos murieron
    print("Ya no queda espacio.")
    break;
  }
  clusterExport(cluster, "actual")
  siguiente <- parSapply(cluster, 1:num, paso)
  
  actual <- matrix(siguiente, nrow=dim, ncol=dim, byrow=TRUE)
  salida = paste("p2_t", iteracion, ".png", sep="")
  tiempo = paste("Paso", iteracion)
  png(salida)
  plot.sociomatrix(actual, diaglab=FALSE, main=tiempo)
  graphics.off()
}
stopCluster(cluster)
for(a in 1:nucleos){
  aux <- c(aux, sum(actual == prob.nucleos[a]))
}
datos <- matrix(aux)
row.names(datos) <- pos.nucleos
barplot(t(datos), xlab = "Posicion inical en matriz")