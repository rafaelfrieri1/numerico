


def optPuntos( lista, listy ):
    promx = []
    promy = []
    for i in range(1,len(lista),2):
        promx.append( (lista[i-1]+lista[i])/2  )

        promy.append( (listy[i-1]+listy[i])/2  )

    return promx,promy

def impR(lis):
    enR = 'c('
    for i in range(len(lis)):
        enR += str(lis[i])
        if(i < len(lis)):
            enR += ', '

    enR += ')'
    print(enR)

def div(lis):
    valid = [10, 15, 27, 37, 50, 56, 70]
    enR = 'c('
    for i in range(len(lis)):
        enR += str(lis[i])
        if(i < len(lis)):
            enR += ', '
        if (i in valid):
            enR += '\n'
    
    print(enR)


#x=[14.65, 14.71, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.52, 17.3, 16.8, 15.4, 14.83, 14.4, 14.5, 15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3, 9.10, 8.6, 7.5, 7.0, 6.7, 6.6, 7.70, 8.00, 8.10, 8.40,9.20, 9.30, 10, 10.2, 10.3, 10.0, 9.50]
#y=[14.7, 14.33, 13.4, 12.33, 11.0, 10.5, 10.22, 8.20, 7.10, 6.70, 6.60, 6.80, 8.30, 8.80, 9.30, 8.80, 6.30, 5.50, 5.00, 4.70, 4.60, 4.50, 4.90, 5.40, 5.80, 6.90, 8.20, 7.60, 5.80, 4.50, 4.30, 3.90, 4.20, 5.70, 7.00, 7.90, 8.20, 7.30, 6.70, 5.50, 5.10, 4.60, 4.7, 5.0, 5.5, 7.2, 7.8, 8.60, 9.40, 10.0, 10.7, 9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 12.3, 12.5, 13.0,13.91, 14.9, 16, 16.4, 16.8, 10.7, 11.0]     


x = [14.65, 14.71, 14.6, 14.8, 15.2, 15.6, 15.7, 17.0, 17.6, 17.52, 17.3, 
  16.8, 15.4, 14.83, 14.4, 14.5, 
  15.0, 15.1, 15.0, 14.9, 14.6, 14.3, 14.0, 13.9, 13.8, 13.5, 13.1, 13.0, 
  13.3, 13.2, 13.1, 12.9, 12.4, 11.9, 11.7, 11.6, 11.3, 10.9, 
  10.7, 10.6, 10.6, 10.1, 9.7, 9.4, 9.3, 9.6, 9.9, 10.1, 10.2, 10.3,  10.0, 9.5, 
  8.6, 7.5, 7.0, 6.7, 6.6, 7.7, 
  8.0, 8.1, 8.4, 9.2, 9.3, 10, 10.2, 10.3]

y = [14.7, 14.33, 13.4, 12.33, 11.0, 10.5, 10.22, 8.2, 7.1, 6.7, 6.6, 
  6.8, 8.3, 8.8, 9.3, 8.8, 
  6.3, 5.5, 5.0, 4.7, 4.6, 4.5, 4.9, 5.4, 5.8, 6.9, 8.2, 7.6, 
  5.8, 4.5, 4.3, 3.9, 4.2, 5.7, 7.0, 7.9, 8.2, 7.3, 
  6.7, 5.5, 5.1, 4.6, 4.7, 5.0, 5.5, 7.2, 7.8, 8.6, 9.4, 10.0,  10.7, 11.0, 
  9.9, 9.0, 9.1, 9.3, 9.7, 11.7, 
  12.3, 12.5, 13.0, 13.91, 14.9, 16, 16.4, 16.8]
#x = [14.68, 14.7, 15.399999999999999, 16.35, 17.560000000000002, 17.05, 15.115, 14.45, 15.05, 14.95, 14.45, 13.95, 13.65, 13.05, 13.25, 13.0, 12.15, 11.649999999999999, 11.100000000000001, 10.649999999999999, 10.35, 9.55, 9.45, 10.0, 10.25, 8.85, 7.25, 6.65, 7.85, 8.25, 9.25, 10.1, 10.15 ]
#y = [14.515, 12.865, 10.75, 9.21, 6.9, 6.699999999999999, 8.55, 9.05, 5.9, 4.85, 4.55, 5.15, 6.35, 7.8999999999999995, 5.15, 4.1, 4.95, 7.45, 7.75, 6.1, 4.85, 4.85, 6.35, 8.2, 9.7, 10.3, 9.05, 9.5, 12.0, 12.75, 14.405000000000001, 16.2, 13.75 ]

a, b  = optPuntos(x,y)

#div(x)
#print()
#div(y)

impR(a)
print()
impR(b)