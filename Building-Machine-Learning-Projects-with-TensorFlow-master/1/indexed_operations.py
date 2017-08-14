import tensorflow as tf 
sess = tf.InteractiveSession()
x = tf.constant([[2, 5, 3, -5], 
                 [0, 3,-2,  5], 
                 [4, 3, 5,  3], 
                 [6, 1, 4,  0]]) 
listx = tf.constant([1,2,3,4,5,6,7,8])
listy = tf.constant([4,5,8,9])

boolx = tf.constant([[True,False], [False,True]])

tf.argmin(x, 1).eval() # Position of the maximum value of columns
tf.argmax(x, 1).eval() # Position of the minimum value of rows
tf.setdiff1d(listx, listy)[0].eval() # List differences
tf.where(boolx).eval() # Show true values
tf.unique(listx)[0].eval() # Unique values in list
