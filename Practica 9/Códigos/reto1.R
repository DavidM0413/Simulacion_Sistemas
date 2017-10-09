library(ggplot2)
n <- 50
p <- data.frame(x = rnorm(n), y=rnorm(n), c=rnorm(n), m= (rexp(n, rate = 1.2) +1))
xmax <- max(p$x)
xmin <- min(p$x)
p$x <- (p$x - xmin) / (xmax - xmin) # ahora son de 0 a 1
ymax <- max(p$y)
ymin <- min(p$y)
p$y <- (p$y - ymin) / (ymax - ymin) # las y tambien
cmax <- max(p$c)
cmin <- min(p$c)
p$c <- 2 * (p$c - cmin) / (cmax - cmin) - 1 # cargas son entre -1 y 1
p$g <- round(5 * p$c) # coloreamos segun la carga a 11 niveles de -5 a 5
densidad <- 1.2
radios <- c()
for(r in 1:n){
  v <- p$m[r]/densidad
  radios <- c(radios, ((3/4*pi)*(v))^(1/3))
}
p$r <- radios
radios <- (radios - min(radios)) / (max(radios) - min(radios))
ta.max <- 2.5
ta.min <- .4
tamaños <- c()
for(t in 1:n){
  tamaños <- c(tamaños,ta.max*radios[t] + (1-radios[t])*ta.min )
}
paso <- floor(256 / 10)
niveles <- seq(0, 255, paso)
colores <- rgb(niveles, rep(0, 11), rev(niveles), max=255)
png("p9i.png")
library(lattice)
xyplot(y ~ x, group=g, data=p, auto.key=list(space="right"),
       xlab="X", ylab="Y", main="Part\u{00ed}culas generadas",
       par.settings = list(superpose.symbol = list(pch = 16, cex = tamaños,
                                                   col = colores)))
graphics.off()
eps <- 0.001
fuerza <- function(i) {
  xi <- p[i,]$x
  yi <- p[i,]$y
  ci <- p[i,]$c
  fx <- 0
  fy <- 0
  mi <- p[i,]$m
  for (j in 1:n) {
    cj <- p[j,]$c
    dir <- (-1)^(1 + 1 * (ci * cj < 0))
    dx <- xi - p[j,]$x
    dy <- yi - p[j,]$y
    factor <- dir * abs(ci - cj) / (sqrt(dx^2 + dy^2) + eps)
    fx <- fx - dx * factor
    fy <- fy - dy * factor
  }
  return(c(fx, fy)/(mi))
}
suppressMessages(library(doParallel))
registerDoParallel(makeCluster(detectCores() - 1))
#system("rm -f p9_t*.png") # borramos anteriores en el caso que lo hayamos corrido
tmax <- 100
digitos <- floor(log(tmax, 10)) + 1
tl <- "0"
while (nchar(tl) < digitos) {
  tl <- paste("0", tl, sep="")
}
png(paste("p9_t", tl, ".png", sep=""))
print(qplot(x=p$x, y=p$y, color=colores[p$g +6], size=p$r, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1), main="Estado inicial", xlab="X", ylab="Y"))
graphics.off()


for (iter in 1:tmax) {
 aux <- c()
  f <- foreach(i = 1:n, .combine=c) %dopar% fuerza(i)
  delta <- 0.02 / max(abs(f)) # que nadie desplace una paso muy largo
  
  p$x <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$x + delta * f[c(TRUE, FALSE)][i], 1), 0)
  p$y <- foreach(i = 1:n, .combine=c) %dopar% max(min(p[i,]$y + delta * f[c(FALSE, TRUE)][i], 1), 0)
  
  tl <- paste(iter, "", sep="")
  while (nchar(tl) < digitos) {
    tl <- paste("0", tl, sep="")
  }
  png(paste("p9_t", tl, ".png",  sep=""))
  print(qplot(x=p$x, y= p$y, color=colores[p$g+6], size =tamaños, xlim=c(-0.1, 1.1), ylim=c(-0.1, 1.1),main=paste("Paso", iter), xlab="X", ylab="Y"))
  graphics.off()
}
stopImplicitCluster()



