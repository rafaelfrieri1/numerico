# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(x) - pi*x

# Halla la raiz de Fx
biseccion <- function(a,b) {
  x<-seq(a,b,0.001)
  x<-b
  i<-0
  error<-abs(a-b)/2
  while (error > 1.e-8 && Fx(x) != 0) {
    i<-i+1
    if (Fx(x)*Fx(a) < 0) b <- x else {a <- x}
    x<-(a+b)/2
    error<-abs(a-b)/2
    cat("X=",x,"\tE=",error,"\n")
  }
  cat("Número de iteraciones:",i,"\n")
}
options(digits=8)
biseccion(0,1)
options(digits=9)
biseccion(1,2)