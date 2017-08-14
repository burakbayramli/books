import tensorflow as tf 
sess = tf.InteractiveSession()

x = tf.constant([[2, 5, 3, -5], 
                 [0, 3,-2,  5], 
                 [4, 3, 5,  3], 
                 [6, 1, 4,  0]]) 
                 
y = tf.constant([[4, -7, 4, -3, 4], 
                 [6, 4,-7,  4, 7], 
                 [2, 3, 2,  1, 4], 
                 [1, 5, 5,  5, 2]])

floatx = tf.constant([[2., 5., 3., -5.], 
                      [0., 3.,-2.,  5.], 
                      [4., 3., 5.,  3.], 
                      [6., 1., 4.,  0.]]) 
                 
tf.transpose(x).eval() # Transpose matrix
tf.matmul(x, y).eval() # Matrix multiplication
tf.matrix_determinant(floatx).eval() # Matrix determinant
tf.matrix_inverse(floatx).eval() # Matrix inverse
tf.matrix_solve(floatx, [[1],[1],[1],[1]]).eval() # Solve Matrix system

