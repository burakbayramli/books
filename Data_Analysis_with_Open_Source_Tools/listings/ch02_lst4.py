
import numpy as np

# Create a 12 element vector and reshape into 3x4 matrix
d = np.linspace( 0, 11, 12 )
d.shape = ( 3,4 )
print d


# Slicing...
# First row
print d[0,:]

# Second col
print d[:,1]


# Individual element: scalar
print d[0,1]

# Sub-vector of shape 1
print d[0:1,1]

# Sub-array of shape 1x1
print d[0:1,1:2]


# Indexing...
# Integer indexing: third and first column
print d[ :, [2,0] ]

# Boolean indexing: second and third column
k = np.array( [False, True, True] )
print d[ k, : ]

