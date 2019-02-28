# Remueve todos los objetos creados
rm(list=ls())
#Fx <- function(x) exp(x) - pi*x
Fx <- function(x) sin(pi*x)
bis <- function(a,b,it)
{ 
  it <- it +1 
  error<-abs(a-b)/2
  
  cat("\n\n\n")

  cat("a",abs(Fx(a)), "    b",abs(Fx(b)))
  if (abs(Fx(a)) > abs(Fx(b)))
  {
    print("cambio a ")
    a <- Fx(a)
  }
  else
  {
    print("cambio b")
    b <- Fx(b)
  }
  cat("(",a,b,")\tE=",error,"\n")
  if ( error > 1.e-8 && Fx(a-b) != 0)# && it < 50)
  {
    return( bis(a,b,it))
    #cat(Fx(x), "    ", Fx(a), "\n")
    #if (Fx(x)*Fx(a) < 0) { return(bis(a,x,it)) } 
    #else                 { return(bis(x,b,it)) }
  }
  return(it)
}
biseccion <- function(a,b) { print(bis(a,b,0)) }
options(digits=8)

biseccion(-1,0)
#biseccion(2,3)
