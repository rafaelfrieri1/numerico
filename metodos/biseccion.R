# Remueve todos los objetos creados
rm(list=ls())
Fx <- function(x) exp(x) - pi*x

bis <- function(a,b,it)
{ 
  it <- it +1 
  error<-abs(a-b)/2
  x<-(a+b)/2
  cat("X=",x,"\tE=",error,"\n")
  if ( error > 1.e-8 && Fx(x) != 0)
  {
    #cat(Fx(x), "    ", Fx(a), "\n")
    if (Fx(x)*Fx(a) < 0) { return(bis(a,x,it)) } 
    else                 { return(bis(x,b,it)) }
  }
  return(it)
}
biseccion <- function(a,b) { print(bis(a,b,0)) }
options(digits=8)
biseccion(0,1)
options(digits=9)
biseccion(1,2)
