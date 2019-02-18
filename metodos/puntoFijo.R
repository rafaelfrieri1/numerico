#f->función sobre la que hay que se encuentran las raices 
f <- function(x)
{
  return (exp(x)/pi)
}

#a -> valor sobre el que se efectua la función,
#b -> valor de la iteración anterior,
#c -> contador / contar la cantidad de iteraciones 
pf <- function(a,b,c)
{
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

puntof <- function(a,b)
{
  print(f(a)-a)
  print(f(b)-b)
  if ( f(a)-a > 0 && f(b)-b<0 )
  {
    return ( pf(a,a-1,0) )
  }
  return(NaN)
}

puntof(0.2,7)