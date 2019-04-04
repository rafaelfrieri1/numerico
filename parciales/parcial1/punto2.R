
F1x <- function(x) tan(pi*x)
F2x <- function(x) cos(pi*x)
h <- function(x) F1x(x) - F2x(x)

iter <- function(a,ant,E)
{
  error <- (h(a)-h(ant))/h(a)
  #error <- h(a-ant)/h(a)
  sig <- a - (h(a) * (a-ant))/(h(a)-h(ant))
  cat("\t",sig,"\t\t",a,"\t\t",error,"\t\t",E,"\n")
  if ( abs(error) > E )
  {
    #sig <- a - (h(a) * (a-ant))/(h(a)-h(ant))
    #cat("\t",sig,"\t\t",a,"\t\t",error,"\t\t",E,"\n")
    return(iter(sig,a,E))
  }
  else 
  {
    return(a)  
  }
}

interseccion <- function (a,E) ## a y b son los limites 
{
  cat("\t siguiente \t anterior \t error\n")
  return (iter(a,0,E))
}

E <- 1.e-4
#options (digits = 4)
val <- interseccion(3,E)
cat("retorno ",val," con valor sobre la funcion h de :",h(val),"\n")

val <- interseccion(pi,E)
cat("retorno ",val," con valor sobre la funcion h de :",h(val),"\n")