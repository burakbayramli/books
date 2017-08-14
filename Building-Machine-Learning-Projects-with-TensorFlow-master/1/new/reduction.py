import tensorflow as tf
sess = tf.InteractiveSession()
x = tf.constant([[1,  2, 3], 
                 [3,  2, 1], 
                 [-1,-2,-3]])
                 
boolean_tensor = tf.constant([[True,  False, True], 
                 [False, False, True], 
                 [True, False, False]])    
            
tf.reduce_prod(x, reduction_indices=1).eval() # reduce prod
tf.reduce_min(x, reduction_indices=1).eval() # reduce min
tf.reduce_max(x, reduction_indices=1).eval() # reduce max
tf.reduce_mean(x, reduction_indices=1).eval() # reduce mean
tf.reduce_all(boolean_tensor, reduction_indices=1).eval() # reduce all 
tf.reduce_any(boolean_tensor, reduction_indices=1).eval() # reduce any
