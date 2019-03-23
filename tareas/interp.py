

import numpy as np
import matplotlib.pyplot as plt

def interp(arr, res):
		val = 0
		mat = [ [ 0 for j in range( len( arr ) + 1 ) ] for i in range( len( arr ) ) ]

		for i in range ( len( arr ) ):
			for j in range( len( arr ) ):
				mat[i][j] = float(arr[i]**j)
			mat[i][len(arr)] = float( res[i] )

		print(np.array(mat))



x = np.array([6,8,10,12,14,16,18,20])
y = np.array([8.5,9.2,12.7,18.4,21.6,17.9,11,9])

print(y)


print(interp( x[3:7] ,y[3:7] ) )


#plt.plot(y)

#plt.show()


