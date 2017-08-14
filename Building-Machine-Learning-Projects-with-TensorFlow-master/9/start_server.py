import tensorflow as tf
tf.app.flags.DEFINE_string("index", "0","Server index")
FLAGS = tf.app.flags.FLAGS
print FLAGS.index
#cluster = tf.train.ClusterSpec({"local": ["ec2-52-90-57-240.compute-1.amazonaws.com:2222", "ec2-54-196-135-128.compute-1.amazonaws.com:2222"]})
cluster = tf.train.ClusterSpec({"local": ["localhost:2222", "localhost:2223"]})
server = tf.train.Server(cluster, job_name="local", task_index=int(FLAGS.index))
server.join()
