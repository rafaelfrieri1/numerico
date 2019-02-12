
#Remueve objetos del entorno y crea función a la que se le hallará las raices
f <- function(x)
{
  return (5*x-exp(x)-1)
}
#f->función a la que hay que encontrarle las raices 
#a->valor sobre el que se efectua la función, b -> valor de la iteración anterior, c-> contador
pf <- function(a,b,c)
{
  print(paste0("::",a, " | ", b) )
  
  
  
  error=abs(a-b)/abs(a)
  cat("X=",a,"\tE=",error,"\n")
  if(error < 0.000000001 || a == -Inf || a == Inf)
  {
    return(a)
  }
  else
  {
    pf(f(a),a,c+1)
  }
}

graf <- function(a,b)
{
  a<- seq(a,b,0.001)
  plot(a , f(a), type="l",col="blue")
  abline(h=0,col="green")
}

puntof <- function(a,b)
{
 # graf(a,b)
  return ( pf(a,a-1,0) )
#  print(f(a)-a)
#  print(f(b)-b)
#  if ( f(a)-a > 0 && f(b)-b<0 )
#  {
#    
#  }
  
  return(NaN)
  
}

puntof(0.2,7)
graf(0.2,7)
