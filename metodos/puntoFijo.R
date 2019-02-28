#f->función sobre la que hay que se encuentran las raices 
f <- function(x){  return (exp(x)/pi)}

#a -> valor sobre el que se efectua la función,
#b -> valor de la iteración anterior,
#c -> contador / contar la cantidad de iteraciones 
pf <- function(a,b,c,E)
{
  error = abs(a-b)/abs(a)
  cat("X=",a,"\tE=",error,"\n")
  if(error < E || a == -Inf || a == Inf)
  {
    return(a)
  }
  else
  {
    pf(f(a),a,c+1,E)
  }
}

puntof <- function(a,b,E)
{
  return ( pf(a,a-1,0,E) )
  
}
E <- 0.000000001
puntof(0.2,7,E)
