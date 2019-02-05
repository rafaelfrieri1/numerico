
f <- function(x)
{
  return ((x^2)-2)
}

bis <-function(a,b,iter)
{
  iter <- iter +1 
  
  if ( a > b )
  {
    c <- a
    a <- b
    b <- c
  }
  
  c = (a+b)/2
  print(c)
  # se acerca hasta el valor esperado /  a y b estan muy cercanos
  
  if(abs(f(c)) <= 1E-10)
  {
    print("n iteraciones "+iter)
    return (c)
  }
  
  if(f(a)* f(c) < 0 )
  {
    return(bis(a,c,iter))
  }
  if(f(c)* f(b) < 0 )
  {
    return(bis(c,b,iter))
  }
}

main <- function(a,b)
{
  raiz = bis(a,b,0)
  print(raiz)
}