aux <- c()
for(k in 1:8){
repetir <- 70
duracion <- 80

library(parallel)

cluster <- makeCluster(detectCores() - 1)
clusterExport(cluster, "duracion")
datos <-  data.frame()

for (dimension in 1:8) {
  clusterExport(cluster, "dimension")
   resultado <- parSapply(cluster, 1:repetir,
                         function(r) {
                           pos <- rep(0, dimension)
                           conteo <- 0
                           for (t in 1:duracion) {
                             cambiar <- sample(1:dimension, 1)
                             cambio <- 1
                             if (runif(1) < 0.5) {
                               cambio <- -1
                             }
                             pos[cambiar] <- pos[cambiar] + cambio
                             origen <- rep(0, dimension)
                             
                             if(all(pos == origen)){
                               conteo <- conteo +1
                             }
                           }
                           return(conteo)
                         })
  datos <- rbind(datos, resultado)
}
suma <- rep(0,dimension)
for(i in 1:dimension){
  suma[i] <- sum(as.numeric(datos[i,]))
}
aux <- c(aux, suma)

stopCluster(cluster)}
respuesta <- cbind(aux, c(1:dimension))
plot(density(respuesta[,1]))

shapiro.test(respuesta[,1])

kruskal.test(respuesta[,1] ~ respuesta[,2])
