import tensorflow as tf
sess = tf.InteractiveSession()
x = tf.constant([[2, 5, 3, -5], 
                 [0, 3,-2,  5], 
                 [4, 3, 5,  3], 
                 [6, 1, 4,  0]]) 
   
tf.shape(x).eval() # Shape of the tensor
tf.size(x).eval() # size of the tensor
tf.rank(x).eval() # rank of the tensor
tf.reshape(x, [8, 2]).eval() # converting to a 10x2 matrix
tf.squeeze(x).eval() #  squeezing
tf.expand_dims(x,1).eval() #Expanding dims
