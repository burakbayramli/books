
import numpy as np

# Generate two vectors with 12 elements each
d1 = np.linspace( 0, 11, 12 )
d2 = np.linspace( 0, 11, 12 )

# Reshape the first vector to a 3x4 (row x col) matrix
d1.shape = ( 3, 4 )
print d1

# Generate a matrix VIEW to the second vector
view = d2.reshape( (3,4) )

# Now: possible to combine the matrix and the view
total = d1 + view


# Element access: [row,col] for matrix
print d1[0,1]
print view[0,1]

# ... and [pos] for vector
print d2[1]


# Shape or layout information
print d1.shape
print d2.shape
print view.shape

# Number of elements (both commands equivalent)
print d1.size
print len(d2)

# Number of dimensions (both commands equivalent)
print d1.ndim
print np.rank(d2)

