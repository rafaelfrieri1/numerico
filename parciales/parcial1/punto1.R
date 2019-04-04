
# encuentra la cantidad de operaciones ocurridas con gauss 
multip <- function(x) # x ->tamaño, se supone es una matriz cuadrada
{
  cant <- 0
  for (i in x:1)
  {
    print(i)
    cant <- cant + i
    print(cant)
    print("-----")
  }
  print(cant)
}

gauss <- function(x,tam)
{
  for (i in tam)
  {
    print
  }
}

multip(4)


b = c(4,5,6,0)


orig = matrix(
  c(
    2.6,0.3,2.4,6.2,
    7.7,0.4,4.7,1.4,
    5.1,9.9,9.5,1.5,
    6.0,7.0,8.5,4.8
  ), nrow= 4 , byrow = TRUE
)
print(orig)



modif = matrix(
  c(
    2.6,0.3,2.4,6.2,
    7.7,0.4,4.7,1.4,
    5.1,9.9,9.5,1.5,
    6.1,7.0,8.5,4.8
  ), nrow= 4 , byrow = TRUE
)
print(modif)

respa <- solve(orig,b) ## se hace gauss sobre las matrices 
respb <- solve(modif,b)

cat ("",respa,"\n",respb,"\n la modificacion en el resultado fue :", respb-respa)


