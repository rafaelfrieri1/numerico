# Remueve todos los objetos creados
rm(list=ls())
#Fx <- function(x) exp(x) - pi*x
Fx <- function(x) sin(pi*x)
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
  else
  {
    cat("\n",it," iteraciones \n" )
    #return(it)
    return(x) 
  }
}
biseccion <- function(a,b) {
  #if (Fx(a)-a > 0 && Fx(b) -b <0 )
  {
    #print(bis(a,b,0)) 
    bis(a,b,0) 
  }
}
options(digits=8)

val <-(biseccion(-1,0))

cat(val,"  " , Fx(val),"\n")

#biseccion(2,3)
