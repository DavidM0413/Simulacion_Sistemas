library(parallel)
n = 500
u = 15
l = 5
radios <-  runif(n, l, u)

x <- seq(0, 50,by= 2.5)
y <- seq(0, 100, length = length(x))
malla <- merge(x = as.data.frame(x), y = as.data.frame(y))
malla <- cbind(malla, c= NA)

x <- c(rnorm(45, mean = 13, sd = 7), rnorm(36, mean = 5, sd = 4),
       rnorm(22, mean = 23, sd = 4), rnorm(56, mean = 13, sd = 6),
       rnorm(67, mean = 42, sd = 4), rnorm(44, mean = 30, sd = 4.5),
       rnorm(56, mean = 45, sd = 4), rnorm(30, mean = 10, sd = 4), rnorm(25, mean = 40, sd = 5))

y <- c(rnorm(45, mean = 90, sd = 5), rnorm(36, mean = 16, sd = 5), 
       rnorm(22, mean = 16, sd = 5), rnorm(56, mean = 48, sd = 4), 
       rnorm(67, mean = 23, sd = 8), rnorm(44, mean = 60, sd = 5), 
       rnorm(56, mean = 78, sd = 10), rnorm(30, mean = 70, sd = 4), rnorm(25, mean = 40, sd = 4))

ubicaciones<- cbind(x, y)
ubicaciones<- as.data.frame(ubicaciones)


cuantos <- function(r, malla, punto){
  n <- dim(malla)[1]
  count <- 0
  for(j in 1:n){
    if((malla[j,1] - punto[1])^2 + (malla[j,2] - punto[2])^2 <= (r)^2){
      if(is.na(malla[j,3])){
        count <- count + 1
      }
    }
  }
  return(count)
}

llenar <- function(r, malla, punto){
  n <- dim(malla)[1]
  
  for(j in 1:n){
    if((malla[j,1] - punto[1])^2 + (malla[j,2] - punto[2])^2 <= (r)^2){
      if(is.na(malla[j,3])){
        malla[j,3] = 1
      }
    }
  }
  return(malla)
}

contador <- function(i){
  return(cuantos(r, malla, punto = as.numeric(ubicaciones[i,])))
}

cluster <- makeCluster(detectCores()- 1)
ya <- F
solucion <- data.frame()
#colnames(solucion) <- c("r", "x", "y")
while(!ya){
  r <- max(radios)
  clusterExport(cluster, c("r", "ubicaciones", "cuantos", "malla", "contador"))
  num <- parSapply(cluster, 1:dim(ubicaciones)[1], contador)
  ubicaciones.p <- ubicaciones[(num == max(num)),]
  punto <- as.numeric(ubicaciones.p[round(runif(1, 1, dim(ubicaciones.p)[1])),])
  malla <- llenar(r, malla, punto)
  solucion <- rbind(solucion, c(r, punto))
  ubicaciones <- subset(ubicaciones, (ubicaciones[,1] != punto[1] & ubicaciones[,2] != punto[2]))
  radios <- radios[(radios != r)]
  ya <- sum(!is.na(malla$c)) == dim(malla)[1]
}
stopCluster(cluster)

#hasta aqui encuentra solucion
