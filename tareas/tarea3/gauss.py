import math
import numpy as np

e = math.exp(1)

A = np.array(((1/3,math.pi,e),(math.sqrt(2), 5 ,0.45)))



#B = np.array((e , 0.45 ))

print(A)


print ("----------------")

A[0] = A[0] / A[0][0]

print(A)
print ("----------------")

A[1] = A[1]-A[0]*A[1][0]

print(A)

A[1] = A[1]/ A[1][1]

print(A)

A[0] = A[0] - A[1]*A[0][1]


print ("----------------")
print(A)

v = (1/3)*A[0][2]
a = (math.pi*A[1][2])
val = ( v + a ) 

err = 0.000001
print( v,a, " ==> ",val,"  ==  ", e )