import tensorflow as tf
import numpy as np 

tf.app.flags.DEFINE_integer("numsamples", "100","Number of samples per server")
FLAGS = tf.app.flags.FLAGS

print ("Sample number per server: " + str(FLAGS.numsamples)	)
cluster = tf.train.ClusterSpec({"local": ["ec2-52-90-57-240.compute-1.amazonaws.com:2222", "ec2-54-196-135-128.compute-1.amazonaws.com:2222"]})

c=[]

def generate_sum():
        i=tf.constant(np.random.uniform(size=FLAGS.numsamples*2), shape=[FLAGS.numsamples,2])
        distances=tf.reduce_sum(tf.pow(i,2),1)
        return (tf.reduce_sum(tf.cast(tf.greater_equal(tf.cast(1.0,tf.float64),distances),tf.int32)))


with tf.device("/job:local/task:0"):
        test1= generate_sum()

with tf.device("/job:local/task:1"):
        test2= generate_sum()

with tf.Session("grpc://ec2-52-90-57-240.compute-1.amazonaws.com:2222") as sess:
      result = sess.run(tf.cast(test1 + test2,tf.float64)/FLAGS.numsamples*2.0)
      print(result)
