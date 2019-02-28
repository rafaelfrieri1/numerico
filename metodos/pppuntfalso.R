# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(-x) + x -2
F1x <- function(x) 1-exp(-x)
# Halla la raiz de Fx
regula <- function(a,b) 
{
  x<-seq(a,b,0.1)
  plot(x,Fx(x),type="l",col="blue")
  abline(h=0,col="blue")
  #x<-b
  #d<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
  ant <- x
  error <- 1
  while (error > 1.e-8 && Fx(x) != 0 ) 
  {
    x<-(Fx(b)*a-Fx(a)*b)/(Fx(b)-Fx(a))
    
    if (Fx(x)*Fx(a) < 0) {b <- x}
    else {a <- x}
    #error <- abs(x-ant) / abs(x)
    error<-abs(Fx(x)/F1x(x))
    points(rbind(c(x,0)),pch=19,cex=0.7,col="red")
    cat("X=",x,"\t","E=",error,"\n")
    ant <- x
  }
}
regula(0,3)

A = matrix(c( 0,  2,  34, 3,
              -5, -4,  1, 4,
              0,  0,  5, 3,
              -4, -7, -8, 0), nrow=4, byrow=TRUE)
b = c(1,0,7,0)

solve(A,b)
