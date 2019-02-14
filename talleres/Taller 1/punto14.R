

#Fx <- function(x){(exp(x))}
Fx <- function(x){  5*x-exp(x)-1}

#Fx <- function(t){sqrt((2*cos(t)-2)^2+(sin(t)-1)^2)}

func <- function(a,b)
{  
  E<-0.000000001

  print(Fx(a))
  print(Fx(b))
  
  print("inicio")

  if(b<a) # b tiene que ser mayor a "a"
  {
    temp<-a
    a<-b
    b<-temp
  }

  return(funcrec(a,b,a,(b-a)/10,E))
}

funcrec <- function(ini,fin,pos,mov,E)
{
  if ( mov < E)
  {
    return(pos)
  }
  else
  {
    if(pos < ini || pos > fin)
    {
      print("rango superado")
      return(NaN)
    }


    if( abs(Fx(pos)) < 1 ) # si esta cerca, pues que se acerque mas 
    {
      mov <- abs(Fx(pos))*mov
    }
    else
    {
      mov <- (fin-pos)/10
    }
    
    cat("valor :",Fx(pos),"\t\tmovimiento:",mov,"\t\tError aceptado: ",E,"\t\taccion: ")
    if(Fx(pos) < 0)
    {
      print("suma")
      return(funcrec(ini,fin,pos+mov,mov,E))
    }
    else if( 0 < Fx(pos) )
    {
      print("resta")
      return(funcrec(ini,fin,pos-mov,mov,E))
    }
    else
    {
      # puro caso hipotetico en que caiga en el valor exacto
      return(pos)
    }
  }
}


resp <- func(-20,5)

print("el resultado es:")
print(resp)

cat("valor sobre la recta : ",Fx(resp))
