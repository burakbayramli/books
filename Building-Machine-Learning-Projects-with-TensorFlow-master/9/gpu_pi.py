import tensorflow as tf
import numpy as np
c = []
#Distribute the work between the GPUs
for d in ['/gpu:0', '/gpu:1', '/gpu:2', '/gpu:3']:
    #Generate the random 2D samples
    i=tf.constant(np.random.uniform(size=10000), shape=[5000,2])
    with tf.Session() as sess:
        tf.initialize_all_variables()
        #Calculate the euclidean distance to the origin
        distances=tf.reduce_sum(tf.pow(i,2),1)
        #Sum the samples inside the circle
        tempsum = sess.run(tf.reduce_sum(tf.cast(tf.greater_equal(tf.cast(1.0,tf.float64),distances),tf.float64)))
        #append the current result to the results array
        c.append( tempsum)
    #Do the final ratio calculation on the CPU
    with tf.device('/cpu:0'):
        with tf.Session() as sess:
            sum = tf.add_n(c)
            print (sess.run(sum/20000.0)*4.0)