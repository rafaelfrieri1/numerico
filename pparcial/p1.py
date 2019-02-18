import numpy as np 

def sumaMatrices( matriz1, matriz2 ):
    
    mat1 = np.array( matriz1 )
    mat2 = np.array( matriz2 )
    
    print(mat1)
    print(mat2)
    #print(mat1.shape[0])

    for i in range(len(mat2)):
        for j in range(len(mat2[i])):
            print(mat1[i][j] , mat2[i][j])
            mat1[i][j] -= mat2[i][j]
    return mat1

def sumancuad(num): # al tener n^2 repeticiones, seria O(n^2)
    cc = 0
    cccuad = 0
    for i in range(num**2 + 1):
        #print(i)
        cc += i
        cccuad += i**2
    return cc, cccuad

mat  = ( (2,3,4) , (1,4,8)  )

mat2 = ( (9,8,7) , (6,5,4) )

mdef = sumaMatrices(mat,mat2)

print(mdef)
print("-----------------------------------")
print(sumancuad(7))
